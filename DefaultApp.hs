module DefaultApp where

import qualified Data.ByteString.Lazy.Char8 as L

defaultJSApp :: String -> String
defaultJSApp js = "<html>\n<head>\n<script type=\"text/javascript\">\nfunction init() {\n"
          ++ js
          ++ "\n};\n</script>\n</head>\n<body onload=\"init()\">\n</body>\n</html>"

defaultHTMLApp :: String -> String
defaultHTMLApp html = "<html>\n<head>\n</head>\n<body>\n" ++ html ++ "</body>\n</html>"