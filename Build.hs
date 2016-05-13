import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (replicateM)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


haxlValues, ohuaValues :: [(Int, Int)]
haxlValues =
    [ (1, 1)
    , (2, 1)
    , (3, 2)
    , (7, 3)
    , (12, 4)
    , (20, 5)
    ]


ohuaValues =
    [ (1, 1)
    , (2, 1)
    , (3, 2)
    , (7, 3)
    , (12, 3)
    , (20, 4)
    ]


smapRounds haxlValues ohuaValues filename =
    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layoutlr_title .= "Accumulator count after transform"
        layoutlr_left_axis . laxis_override .= axisGridHide
        layoutlr_right_axis . laxis_override .= axisGridHide
        plotLeft (line "haxl" [ haxlValues ])
        plotRight (line "yauhau" [ ohuaValues ])


buildDir = "_build"
sourceDir = "src"
chapterDir = "Chapters"


plots = ["smap-rounds.eps"]


buildPFD :: FilePath -> Action ()
buildPFD out = do
    chapters <- map (chapterDir </>) <$> getDirectoryContents (sourceDir </> chapterDir)
    need $ [src] ++ map ((sourceDir </> "Figures") </>) plots
    [_, Exit e] <- replicateM 2 $ command [Cwd sourceDir] "pdflatex" ["-shell-escape", "-interaction=nonstopmode", srcRel]
    -- trackWrite [buildDir </> out]
    copyFileChanged (sourceDir </> out) out
  where
      srcRel =  out -<.> "tex"
      src = sourceDir </> srcRel


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want ["thesis.pdf"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter buildDir ["//*"]

    "*.pdf" %> buildPFD

    "//smap-rounds.eps" %> smapRounds haxlValues ohuaValues
