
dream1 :: String -> [String]
dream1 x =
  [
    x ++ "dream",
    x ++ "dreamer",
    x ++ "erase",
    x ++ "eraser"
  ]

dreamN :: Int -> String -> [String]
dreamN 0 x = [x]
dreamN n x = dream1 x >>= dreamN (n - 1)