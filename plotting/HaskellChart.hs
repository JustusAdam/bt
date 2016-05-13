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


main = toFile (def & fo_format .~ EPS) "example2_big.eps" $ do
    layoutlr_title .= "Accumulator count after transform"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft (line "haxl" [ haxlValues ])
    plotRight (line "yauhau" [ ohuaValues ])
