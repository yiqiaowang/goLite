module CodeGen.Native.Array where


goLiteCopy :: String
goLiteCopy = unlines
  [ "function GO_LITE_COPY(obj) {"
  , "  if (obj.isSlice) return obj;"
  , "  return JSON.parse(JSON.stringify(obj));"
  , "}"
  ]


goLiteAppend :: String
goLiteAppend = unlines
  [ "function GO_LITE_APPEND(slice, addition) {"
  , "\tslice.push(addition);"
  , "\treturn slice;"
  , "}"
  ]

goLiteReadIndex :: String
goLiteReadIndex = unlines
  [ "function GO_LITE_READ_INDEX(arr, index) {"
  , "  GO_LITE_BOUNDS_CHECK(arr, index)"
  , "  return arr[index];"
  , "}"
  ]

goLiteAssignIndex :: String
goLiteAssignIndex = unlines
  [ "function GO_LITE_ASSIGN_INDEX(arr, index, val) {"
  , "  GO_LITE_BOUNDS_CHECK(arr, index);"
  , "  arr[index] = val;"
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
