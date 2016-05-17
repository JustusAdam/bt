module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath as SFP
import Development.Shake.Util
import Control.Monad (replicateM)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


haxlValues, ohuaValues, intervals :: [Int]
(intervals, haxlValues, ohuaValues) = unzip3
    [ (0, 0, 0)
    , (1, 1, 1)
    , (2, 1, 1)
    , (3, 2, 2)
    , (7, 3, 3)
    , (12, 4, 3)
    , (20, 5, 4)
    , (39, 8, 6)
    , (60, 10, 8)
    , (100, 15, 11)
    ]


smapRounds haxlValues ohuaValues filename =
    liftIO $ toFile (def & fo_format .~ EPS) filename $ do
        layout_title .= "Transformation performance"
        layout_x_axis . laxis_title .= "Number of nodes in the program graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        plot (line "haxl" [ zip intervals haxlValues ])
        plot (line "yauhau" [ zip intervals ohuaValues ])


buildDir = "_build"
sourceDir = "src"
chapterDir = "Chapters"
appendixDir = "Appendices"
chapters = ["Context", "Introduction", "Experiments"]
appendices = ["AppendixA"]
styles = ["MastersDoctoralThesis"]


plots = ["smap-rounds.eps"]


copyFromSrc :: FilePath -> Action ()
copyFromSrc out = copyFileChanged (sourceDir </> dropDirectory1 out) out


buildPFD :: FilePath -> Action ()
buildPFD out = do
    need $
      src
      : map (\chapter -> buildDir </> chapterDir </> chapter SFP.<.> "tex") chapters
      ++ map (\appendix -> buildDir </> appendixDir </> appendix SFP.<.> "tex") appendices
      ++ map (\style -> buildDir </> style SFP.<.> "cls") styles
      ++ map ((buildDir </> "Figures") </>) plots
    [_, Exit e] <- replicateM 2 $ command [Cwd buildDir] "pdflatex" ["-shell-escape", "-interaction=nonstopmode", srcRel]
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

    "*.pdf" %> buildPFD

    "//smap-rounds.eps" %> smapRounds haxlValues ohuaValues

    "_build//*.tex" %> copyFromSrc
    "_build//*.cls" %> copyFromSrc
