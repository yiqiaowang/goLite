module CodeGen.Array where


goLiteAppend :: String
goLiteAppend = unlines
  [ "function GO_LITE_APPEND(slice, addition) {"
  , "\tslice.push(addition);"
  , "\treturn slice;"
  , "}"
  ]

goLiteBoundsCheck :: String
goLiteBoundsCheck = unlines
  [ "function GO_LITE_BOUNDS_CHECK(arr, index) {"
  , "  if (index < 0 || index >= arr.length) {"
  , "     throw new Error('INVALID INDEX');"
  , "  }"
  , "}"
  ]
