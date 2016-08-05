{-# LANGUAGE FlexibleContexts #-}
module Main where


import           BT.Types as Types
import           BT.Util
import           ClassyPrelude
import           Control.Monad                             (replicateM)
import           Data.Aeson (eitherDecode, FromJSON)
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath                as SFP
import           Development.Shake.Util
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import Text.Printf


readDecOrFail :: FromJSON a => FilePath -> Action a
readDecOrFail file =
    either (\e -> error $ printf "Error in file %v: %v" file e) id . eitherDecode <$> readFile file


formatRoundData :: MeasuredGraphs -> [(Int, Int)]
formatRoundData = map average . groupAllOn Types.levels
  where
    average grs = (Types.levels (headEx grs), avrg)
      where
        avrg = sum (map rounds grs) `div` length grs


smapRounds filename = do
    need ["plotting/haskell-map.json", "plotting/yauhau-map-monad.json"]

    haxlValues <- readDecOrFail "plotting/haskell-map.json"
    yauhauValues <- readDecOrFail "plotting/yauhau-map-monad.json"

    let all = [("haxl", haxlValues), ("yauhau", yauhauValues)]
        groupedAndSorted = map (second (sortOn fst . formatRoundData)) all

    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Number of levels in the program graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        mapM_ (\(name, data_) -> plot $ line name [data_]) groupedAndSorted


funcRounds filename = do
    need ["plotting/haskell-func.json", "plotting/yauhau-func-monad.json"]

    haxlValues <- readDecOrFail "plotting/haskell-func.json"
    yauhauValues <- readDecOrFail "plotting/yauhau-func-monad.json"

    let all = [("haxl", haxlValues), ("yauhau", yauhauValues)]
        groupedAndSorted = map (second (catMaybes . map (\e -> do
                                                            c <- headMay (e :: MeasuredGraphs) >>= genConf
                                                            p <- prctFuns c
                                                            return (p, sum (map rounds e) `div` length e))
                                                  . groupAllOn (genConf >=> prctFuns))) all

    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Number of levels in the program graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        mapM_ (\(name, data_) -> plot $ line name [data_]) groupedAndSorted


buildDir = "_build"
sourceDir = "src"
chapterDir = "Chapters"
appendixDir = "Appendices"
figuresDir = "Figures"
styles = ["MastersDoctoralThesis.cls"]


plots = ["smap-rounds.eps", "func-rounds.eps"]


copyFromSrc :: FilePath -> Action ()
copyFromSrc out = copyFileChanged (sourceDir </> dropDirectory1 out) out


getTexFilesFrom :: FilePath -> Action [FilePath]
getTexFilesFrom dir = filter ((==) ".tex" . takeExtension) <$> getDirectoryContents dir


buildPFD :: FilePath -> Action ()
buildPFD out = do
    chapters <- getTexFilesFrom $ sourceDir </> chapterDir
    appendices <- getTexFilesFrom $ sourceDir </> appendixDir
    figures <- filter ((/=) ".graffle" . takeExtension) <$> getDirectoryContents (sourceDir </> figuresDir)
    bibliography <- filter ((==) ".bib" .takeExtension) <$> getDirectoryContents sourceDir
    need $
      src
      : map (\chapter -> buildDir </> chapterDir </> chapter) chapters
      ++ map (\appendix -> buildDir </> appendixDir </> appendix) appendices
      ++ map (buildDir </>) styles
      ++ map ((buildDir </> "Figures") </>) (figures ++ plots)
      ++ map (buildDir </>) bibliography
    Exit e <- command [Cwd buildDir] "latexmk" ["-pdf", "-shell-escape", "-interaction=nonstopmode", srcRel]
    -- trackWrite [buildDir </> out]
    copyFileChanged (buildDir </> out) out
  where
      srcRel =  out -<.> "tex"
      src = buildDir </> srcRel


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want ["thesis.pdf"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter buildDir ["//*"]

    "_build/Figures/*.pdf" %> copyFromSrc

    "thesis.pdf" %> buildPFD

    "_build/Figures/smap-rounds.eps" %> smapRounds
    "_build/Figures/func-rounds.eps" %> funcRounds

    "_build//*.tex" %> copyFromSrc
    "_build//*.cls" %> copyFromSrc
    "_build//*.bib" %> copyFromSrc
