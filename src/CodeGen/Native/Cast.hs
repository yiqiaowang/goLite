module CodeGen.Native.Cast where

goLiteIntCast :: String
goLiteIntCast = unlines
  [ "function int(a) {"
  , "  if (typeof(a) == 'string') return a.charCodeAt(0);"
  , "  return Math.floor(Number(a))"
  , "}"
  ]

goLiteFloat64Cast :: String
goLiteFloat64Cast = unlines
  [ "function float64(f) {"
  , "  if (typeof(f) == 'string') return f.charCodeAt(0);"
  , "  return Number(f);"
  , "}"
  ]

goLiteRuneCast :: String
goLiteRuneCast = unlines
  [ "function rune(r) {"
  , "  if (typeof(r) == 'string') return r;"
  , "  return String.fromCharCode(Number(r))"
  , "}"
  ]

goLiteBoolCast :: String
goLiteBoolCast = unlines
  [ "function bool(b) {"
  , "  if (typeof(r) == 'string') return Boolean(b.charCodeAt(0));"
  , "  return Boolean(b);"
  , "}"
  ]
