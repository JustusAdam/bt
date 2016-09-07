{-# LANGUAGE FlexibleContexts  #-}
module Main where


import           ClassyPrelude
import           Control.Monad                             (replicateM)
import           Data.Aeson                                (FromJSON,
                                                            eitherDecode)
import           Data.Maybe                                (fromJust)
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath                as SFP
import           Development.Shake.Util
import           Experiment.Haxl.Types                     as Types
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Drawing
import           Graphics.Rendering.Chart.Easy
import           Text.Printf


readDecOrFail :: FromJSON a => FilePath -> Action a
readDecOrFail file =
    either (error . printf "Error in file %v: %v" file) id . eitherDecode <$> readFile file


fdiv :: (Real a, Real b, Fractional c) => a -> b -> c
fdiv a b = realToFrac a / realToFrac b


levelsToRounds :: MeasuredGraphs -> [(Int, Double)]
levelsToRounds = sortOn fst . map average . groupAllOn Types.levels
  where
    average grs = (Types.levels (headEx grs), avrg)
      where
        avrg = sum (map rounds grs) `fdiv` length grs


percentagesToSomething :: (Ord a, Real b) => (MeasuredGraph -> b) -> (GenConf -> Maybe a) -> MeasuredGraphs -> [(a, Double)]
percentagesToSomething getSomething getter = sortOn fst . catMaybes . map average . groupAllOn (genConf >=> getter)
  where
    average e = do
        c <- headMay e >>= genConf
        p <- getter c
        return (p, sum (map getSomething e) `fdiv` length e)

percentagesToRounds = percentagesToSomething rounds
percentagesToTime = percentagesToSomething time


readData :: FromJSON a => [(b, FilePath)] -> Action [(b, a)]
readData sourceData = do
    need $ map snd sourceData
    mapM (\(name, file) -> (name, ) <$> readDecOrFail file) sourceData


renderWithDefaultStyle :: (Default (Layout a b), ToRenderable (Layout a b), MonadIO m) => FilePath -> EC (Layout a b) () -> m ()
renderWithDefaultStyle filename inner =
      void $ liftIO $ toFile (def & fo_format .~ EPS & fo_size .~ (800, 400)) filename $ do
          layout_x_axis . laxis_title_style . font_size .= 15.0
          layout_x_axis . laxis_style . axis_label_gap .= 10.0
          layout_y_axis . laxis_title_style . font_size .= 15.0
          layout_y_axis . laxis_style . axis_label_gap .= 10.0
          layout_margin .= 30
          inner
--    void $ liftIO $ cBackendToFile (def & fo_format .~ EPS) (withScaleY 0.5 $ toRenderable $ execEC inner) filename


plotAll :: [(String, [(a, b)])] -> EC (Layout a b) ()
plotAll values = do
    setShapes [PointShapeCircle, PointShapeArrowHead 1, PointShapeCross]
    for_ values $ \(name, data_) -> do

        color <- takeColor
        shape <- takeShape
        plot $ liftEC $ do
                plot_lines_title .= name
                plot_lines_values .= [data_]
                plot_lines_style . line_color .= color
        plot $ liftEC $ do
                plot_points_values .= data_
                plot_points_style . point_color .= color
                plot_points_style . point_shape .= shape
                plot_points_style . point_radius .= 4


getAverageDifference :: Action Float
getAverageDifference = do
    haxl <- readDecOrFail "plotting/haskell-vanilla.json"
    yauhau <- readDecOrFail "plotting/yauhau-vanilla-monad.json"
    let differences = map (\((l1, v1), (l2, v2)) -> assert (l1 == l2) $ v2 / v1) $ zip (levelsToRounds haxl) (levelsToRounds yauhau)

    return $ sum differences `fdiv` length differences


smapPrimerSource =
    [ ("Haxl", "plotting/haskell-map-primer.json")
    , ("Yauhau", "plotting/yauhau-map-primer-monad.json")
    ]


smapPrimer filename = do
    values <- readData smapPrimerSource
    let groupedAndSorted = map (second $ percentagesToRounds prctFuns) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Number of levels in the graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        plotAll groupedAndSorted


vanillaExperimentSource =
    [ ("Haxl", "plotting/haskell-vanilla.json" )
    , ("Yauhau", "plotting/yauhau-vanilla-monad.json")
    ]


vanillaExperiment filename = do
    values <- readData vanillaExperimentSource
    let groupedAndSorted = map (second $ percentagesToRounds prctFuns) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Number of levels in the graph"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        plotAll groupedAndSorted


smapExperiment filename = do
    -- let readData base withMap = (,) <$> readDecOrFail base <*> readDecOrFail withMap
    let readData = liftM2 (,) `on` readDecOrFail
    haxl <- readData "plotting/haskell-map-primer.json" "plotting/haskell-map.json"
    yauhau <- readData "plotting/yauhau-map-primer-monad.json" "plotting/yauhau-map-monad.json"
    noMaps <- readDecOrFail "plotting/yauhau-map-primer-monad.json"

    let h1 = mapFromList $ percentagesToRounds prctFuns $ fst haxl :: HashMap Double Double
        y1 = mapFromList $ percentagesToRounds prctFuns $ fst yauhau :: HashMap Double Double

        processedHaxl = map (\(p, v) -> (p, v - fromJust (liftM2 (-) (lookup p h1) (lookup p y1)))) $
          percentagesToRounds prctMaps $ snd haxl
        processedYauhau = percentagesToRounds prctMaps $ snd yauhau
        processedNoMaps = percentagesToRounds prctMaps noMaps

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Percentage of mapping nodes during generation"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        plotAll [("Haxl", processedHaxl), ("Yauhau", processedYauhau), ("No maps (Yauhau)", processedNoMaps)]


ifExperiment filename = do
    raw <- readDecOrFail "plotting/yauhau-if-monad.json"

    let (inline, noinline) = partition (fromJust . (genConf >=> inlineIf)) raw
    let values = [("Inline", inline), ("Precomputed", noinline)]
    let groupedAndSorted = map (second $ percentagesToRounds prctIfs) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Percentage of conditional nodes"
        layout_y_axis . laxis_title .= "Number of rounds performed"
        plotAll groupedAndSorted


ifExperimentFetches filename = do
    raw <- readDecOrFail "plotting/yauhau-if-monad.json"

    let (inline, noinline) = partition (fromJust . (genConf >=> inlineIf)) raw
    let values = [("Inline", inline), ("Precomputed", noinline)]
    let groupedAndSorted = map (second $ percentagesToSomething fetches prctIfs) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Percentage of conditional nodes"
        layout_y_axis . laxis_title .= "Number of fetches performed"
        plotAll groupedAndSorted


ifExperimentExecTime filename = do
    raw <- readDecOrFail "plotting/yauhau-if-monad.json"

    let (inline, noinline) = partition (fromJust . (genConf >=> inlineIf)) raw
    let values = [("Inline", inline), ("Precomputed", noinline)]
    let groupedAndSorted = map (second $ percentagesToTime prctIfs) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Percentage of conditional nodes"
        layout_y_axis . laxis_title .= "Program execution time"
        plotAll groupedAndSorted


ifExperimentDelayed filename = do
    raw <- readDecOrFail "plotting/yauhau-if-delayed-monad.json"

    let (inline, noinline) = partition (fromJust . (genConf >=> inlineIf)) raw
    let values = [("Inline", inline), ("Precomputed", noinline)]
    let groupedAndSorted = map (second $ percentagesToTime prctSlow) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Percentage of conditional nodes"
        layout_y_axis . laxis_title .= "Program execution time"
        plotAll groupedAndSorted

funcExperimentSource =
    [ ("Haxl", "plotting/haskell-func.json")
    , ("Yauhau", "plotting/yauhau-func-monad.json")
    ]

funcExperiment filename = do
    values <- readData funcExperimentSource

    let groupedAndSorted = map (second $ percentagesToRounds prctFuns) values

    renderWithDefaultStyle filename $ do
        layout_x_axis . laxis_title .= "Percentage of function nodes during generation"
        layout_y_axis . laxis_title .= "Number of Fetch rounds performed/accumulators inserted"
        plotAll groupedAndSorted


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
    , "if-experiment-fetches.eps"
    , "if-experiment-exec-time.eps"
    , "if-experiment-delayed.eps"
    , "smap-primer-experiment.eps"
    ]
figures =
    [ "basic-if-rewrite-original.pdf"
    , "basic-if-rewrite.pdf"
    , "basic-if.pdf"
    , "haxl-basic-example.pdf"
    , "if-hypothesised.pdf"
    , "if-in-operators.pdf"
    , "if-in-reality.pdf"
    , "if-insert-empty-better-after-insert.pdf"
    , "if-insert-empty-parallel-after-insert.pdf"
    , "if-insert-empty-parallel-before.pdf"
    , "if-trans-before.pdf"
    , "if-trans-merged.pdf"
    , "naive-transformation.pdf"
    , "ohua-compiler-flow-with-ctx.pdf"
    , "ohua-compiler-flow.pdf"
    , "requests-on-branches-graph.pdf"
    , "requests-precomputed-graph.pdf"
    , "smap-rewrite-original.pdf"
    , "smap-rewrite.pdf"
    , "smap-rounds-eps-converted-to.pdf"
    , "yauhau-rewrite-flow.pdf"
    , "yauhau-round-detection.pdf"
    , "yauhau-transformation.pdf"
    , "context-nesting-example-exploded.pdf"
    , "identity-example.pdf"
    , "redundant-identity-example.pdf"
    , "redundant-identity-example-rewritten.pdf"
    , "ohua-code-example.pdf"
    , "indeterministic-code-example.pdf"
    ]
chapters =
    [ "Context.tex"
    , "Experiments.tex"
    , "Extending-Code-Generator.tex"
    , "If-Transformation.tex"
    , "Introduction.tex"
    , "Ohua.tex"
    , "Related-Work.tex"
    , "Side-Effects.tex"
    , "Smap-Transformation.tex"
    , "Transformation-Implementation-Guidelines.tex"
    , "Yauhau.tex"
    , "Future-Work.tex"
    ]
appendices =
    [ "AppendixA.tex" ]
bibliography = [ "main.bib" ]


copyFromSrc :: FilePath -> Action ()
copyFromSrc out = copyFileChanged (sourceDir </> dropDirectory1 out) out


getTexFilesFrom :: FilePath -> Action [FilePath]
getTexFilesFrom dir = filter ((==) ".tex" . takeExtension) <$> getDirectoryContents dir


buildPFD :: FilePath -> Action ()
buildPFD out = do
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
    "_build/Figures/if-experiment-fetches.eps" %> ifExperimentFetches
    "_build/Figures/if-experiment-delayed.eps" %> ifExperimentDelayed
    "_build/Figures/if-experiment-exec-time.eps" %> ifExperimentExecTime
    "_build/Figures/vanilla-experiment.eps" %> vanillaExperiment
    "_build/Figures/smap-primer-experiment.eps" %> smapPrimer

    "_build//*.tex" %> copyFromSrc
    "_build//*.cls" %> copyFromSrc
    "_build//*.bib" %> copyFromSrc
