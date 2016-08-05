module BT.Util where


import Data.Aeson.Types
import ClassyPrelude


rewritePrefixes :: [(String, String)] -> (String -> String) -> String -> String
rewritePrefixes [] inner source = inner source
rewritePrefixes ((trigger, target):xs) inner source =
    maybe (rewritePrefixes xs inner source) ((target ++) . inner) $ stripPrefix trigger source


gconfPrefixTrans :: [(String, String)]
gconfPrefixTrans =
    [ ("prct", "%")
    , ("num", "#")
    ]

gconfOptions = defaultOptions {fieldLabelModifier= rewritePrefixes gconfPrefixTrans (camelTo '_')}
