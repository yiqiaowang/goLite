module CodeGen.Native.Comparison where


goLiteEquals :: String
goLiteEquals = unlines
  [ "function GO_LITE_EQUALS(obj1, obj2) {"
  , "  return JSON.stringify(obj1) == JSON.stringify(obj2);"
  , "}"
  ]

goLiteNotEquals :: String
goLiteNotEquals = unlines
  [ "function GO_LITE_NOT_EQUALS(obj1, obj2) {"
  , "  return JSON.stringify(obj1) != JSON.stringify(obj2);"
  , "}"
  ]

goLiteIntDiv :: String
goLiteIntDiv = unlines
  [ "function GO_LITE_INT_DIV(x, y) {"
  , "  return Math.floor(x / y);"
  , "}"
  ]
