import Control.Monad

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = flip (.)

hommachiLineCSV :: String
hommachiLineCSV = "駅名, キロ程\n本町, 0.0\n市役所前, 0.7\n城址公園, 1.3\n運動場前, 2.0\n大学北, 2.7\nみなと広場, 3.3\n県営団地, 4.0\n温泉入口, 4.7\n登山口, 5.3"

kukoLineCSV :: String
kukoLineCSV = "駅名, キロ程\n空港前, 0.0\n工業団地, 1.2\n市場前, 1.9\n駅北, 2.5\n駅南, 2.7\n大橋, 3.5"

faresCSV :: String
faresCSV = "運賃, 最小キロ程\n130, 0.0\n170, 1.0\n210, 2.0\n250, 3.0\n290, 4.0\n330, 5.0"

splitOn :: Char -> String -> [String]
splitOn c s =
  case break (== c) s of
    (sl , "")  -> [sl]
    (sl, _:sr) -> sl : splitOn c sr

unwrapSpace :: String -> String
unwrapSpace = dropWhile (== ' ') >>> takeWhile (/= ' ')

readCSV :: String -> Maybe ([String], [[String]])
readCSV str =
  case (splitOn ',' >>> fmap unwrapSpace) <$> lines str of
    header : body -> Just (header, body)
    _             -> Nothing

readMay :: Read a => String -> Maybe a
readMay str =
  case reads str of
    (a, _):_ -> Just a
    _        -> Nothing

readStationCol :: [String] -> Maybe (String, Double)
readStationCol strs =
  case strs of
    name : kilostr : _ ->
      case readMay kilostr of
        Just kilo -> Just (name, kilo)
        Nothing -> Nothing
    _ -> Nothing


readFareCol :: [String] -> Maybe (Int, Double)
readFareCol strs =
  case strs of
    farestr : kilostr : _ ->
      case readMay farestr of
        Just fare ->
          case readMay kilostr of
            Just kilo -> Just (fare, kilo)
            Nothing -> Nothing
        Nothing -> Nothing

readStationsCSV :: String -> Maybe [(String, Double)]
readStationsCSV str =
  case readCSV str of
    Just (_, body) ->
      traverse readStationCol body
    Nothing -> Nothing

readFaresCSV :: String -> Maybe [(Int, Double)]
readFaresCSV str =
  case readCSV str of
    Just (_, body) ->
      traverse readFareCol body
    Nothing -> Nothing

readFareCol' :: [String] -> Maybe (Int, Double)
readFareCol' line =
  case line of
    farestr : kilostr : _ -> (,) <$> readMay farestr <*> readMay kilostr

readStationCol' :: [String] -> Maybe (String, Double)
readStationCol' line =
  case line of
    name : kilostr : _ -> (name, ) <$> readMay kilostr
    _ -> Nothing

readStationsCSV' :: String -> Maybe [(String, Double)]
readStationsCSV' str =
  traverse readStationCol . snd =<< readCSV str 

readFare' :: String -> Maybe [(Int, Double)]
readFare' str =
  traverse readFareCol . snd =<< readCSV str


getStationPos :: [(String, Double)] -> String -> Maybe Double
getStationPos stations name =
  case stations of
    [] -> Nothing
    (stname, kilo) : sts
      | stname == name -> Just kilo
      | otherwise      -> getStationPos sts name

kiloToFare :: [(Int, Double)] -> Double -> Maybe Int
kiloToFare fares kilo =
  case fares of
    (fare, mink):(farenext, minknext):fs
      | kilo < minknext -> Just fare
      | otherwise -> kiloToFare ((farenext, minknext):fs) kilo
    [(fare, _)] -> Just fare
    _ -> Nothing


calcFareKilo :: [(String, Double)] -> [(Int, Double)] -> String -> String -> Maybe (Int, Double)
calcFareKilo stations fares from to =
  let k1 = getStationPos stations from
      k2 = getStationPos stations to
      kilo = (\a b -> abs (a - b)) <$> k1 <*> k2
  in kilo >>= (\k -> (,k) <$> kiloToFare fares k)

calcFareAndKiloFromCSV :: String -> String -> (String -> String -> Maybe (Int, Double))
calcFareAndKiloFromCSV stationsCSV faresCSV from to  =
  case readStationsCSV stationsCSV of
    Just sdata -> 
      case readFaresCSV faresCSV of
        Just fdata -> calcFareKilo sdata fdata from to
        _ -> Nothing
    _ -> Nothing

calcFareFromCSV stationsCSV faresCSV from to =
  fst <$> calcFareAndKiloFromCSV stationsCSV faresCSV from to

calcFareAndKiloHommachi = calcFareAndKiloFromCSV hommachiLineCSV faresCSV

calcFareAndKiloKuko = calcFareAndKiloFromCSV kukoLineCSV faresCSV

-- >>> calcFareAndKiloHommachi "本町" "登山口"
-- Just (330,5.3)