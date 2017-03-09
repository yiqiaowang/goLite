module Pretty.Pretty where


class Pretty a where
  pretty :: a -> Integer -> String
  prettyList :: [a] -> Integer -> String
  prettyList ps i = concatMap (`pretty` i) ps
