module DefaultApp where

import qualified Data.ByteString.Lazy.Char8 as L

defaultJSApp :: String -> String
defaultJSApp jsDyn = "<html>\n<head>\n"
          ++ "<script src='static/closure-library/closure/goog/base.js'></script>"
          ++ "<script src='static/hwt9.js'></script>"
          ++ "<script type=\"text/javascript\">\nfunction init() {\n"
          ++ jsDyn
          ++ "\n};\n</script>\n</head>\n<body onload=\"init()\">\n</body>\n</html>"

defaultHTMLApp :: String -> String
defaultHTMLApp html = "<html>\n<head>\n</head>\n<body>\n" ++ html ++ "</body>\n</html>"
