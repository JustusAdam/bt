{-# LANGUAGE FlexibleContexts #-}
module Main where


import           BT.Types                                  as Types
import           BT.Util
import           ClassyPrelude
import           Control.Monad                             (replicateM)
import           Data.Aeson                                (FromJSON,
                                                            eitherDecode)
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath                as SFP
import           Development.Shake.Util
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           Text.Printf


readDecOrFail :: FromJSON a => FilePath -> Action a
readDecOrFail file =
    either (error . printf "Error in file %v: %v" file) id . eitherDecode <$> readFile file


levelsToRounds :: MeasuredGraphs -> [(Int, Int)]
levelsToRounds = map average . groupAllOn Types.levels
  where
    average grs = (Types.levels (headEx grs), avrg)
      where
        avrg = sum (map rounds grs) `div` length grs


percentagesToRounds :: Ord a => (GenConf -> Maybe a) -> MeasuredGraphs -> [(a, Int)]
percentagesToRounds getter = catMaybes . map average . groupAllOn (genConf >=> getter)
  where
    average e = do
        c <- headMay e >>= genConf
        p <- getter c
        return (p, sum (map rounds e) `div` length e)


readData :: FromJSON a => [(b, FilePath)] -> Action [(b, a)]
readData sourceData = do
    need $ map snd sourceData
    mapM (\(name, file) -> (name, ) <$> readDecOrFail file) sourceData


smapExperimentSource =
    [ ("Haxl", "plotting/haskell-map.json")
    , ("Yauhau", "plotting/yauhau-map-monad.json")
    ]

smapExperiment filename = do
    values <- readData smapExperimentSource

    let groupedAndSorted = map (second levelsToRounds) values

    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Percentage of mapping nodes during generation"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        mapM_ (\(name, data_) -> plot $ line name [data_]) groupedAndSorted

ifExperimentSource =
    [ ("Haxl", "plotting/haskell-if.json")
    , ("Yauhau", "plotting/yauhau-if-monad.json")
    ]

ifExperiment filename = do
    values <- readData ifExperimentSource

    let groupedAndSorted = map (second levelsToRounds) values

    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Number of levels in the program graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        mapM_ (\(name, data_) -> plot $ line name [data_]) groupedAndSorted

ifExperimentDelayedSource =
    [ ("Haxl", "plotting/haskell-if-delayed.json")
    , ("Yauhau", "plotting/yauhau-if-delayed-monad.json")
    ]

ifExperimentDelayed filename = do
    values <- readData ifExperimentDelayedSource

    let groupedAndSorted = map (second levelsToRounds) values

    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Number of levels in the program graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        mapM_ (\(name, data_) -> plot $ line name [data_]) groupedAndSorted

funcExperimentSource =
    [ ("Haxl", "plotting/haskell-func.json")
    , ("Yauhau", "plotting/yauhau-func-monad.json")
    ]

funcExperiment filename = do
    values <- readData funcExperimentSource

    let groupedAndSorted = map (second $ percentagesToRounds prctFuns) values

    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Percentage of function nodes during generation"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        mapM_ (\(name, data_) -> plot $ line name [data_]) groupedAndSorted


buildDir = "_build"
sourceDir = "src"
chapterDir = "Chapters"
appendixDir = "Appendices"
figuresDir = "Figures"
styles = ["MastersDoctoralThesis.cls"]
plots =
    [ "smap-experiment.eps"
    , "func-experiment.eps"
    , "if-experiment.eps"
    -- , "if-experiment-delayed.eps"
    ]


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

    "_build/Figures/smap-experiment.eps" %> smapExperiment
    "_build/Figures/func-experiment.eps" %> funcExperiment
    "_build/Figures/if-experiment.eps" %> ifExperiment
    "_build/Figures/if-experiment-delayed.eps" %> ifExperimentDelayed

    "_build//*.tex" %> copyFromSrc
    "_build//*.cls" %> copyFromSrc
    "_build//*.bib" %> copyFromSrc
