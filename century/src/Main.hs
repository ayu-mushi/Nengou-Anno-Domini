module Main where

import System.Environment(getArgs)
import Data.List (intersperse, transpose, intercalate)
import Data.Char (isAlpha)

yr2cent :: Int -> Int
yr2cent year = ((year-1) `div` 100) + 1

whereinCent :: Year -> Where
whereinCent (BCyear year)
      | ((year-1) `mod` 100) < 10 = Last
      | ((year-1) `mod` 100) < 40 = Latter
      | ((year-1) `mod` 100) > 60 = First
      | otherwise = Half
whereinCent (ACyear year)
      | ((year-1) `mod` 100) > 90 = Last
      | ((year-1) `mod` 100) < 40 = First
      | ((year-1) `mod` 100) > 60 = Latter
      | otherwise = Half

year2cent :: Year -> Century
year2cent y@(BCyear n) = BCcent (yr2cent n) (whereinCent y)
year2cent y@(ACyear n) = ACcent (yr2cent n) (whereinCent y)

data Year = BCyear Int | ACyear Int
data Century = BCcent Int Where | ACcent Int Where

instance Show Year where
  show (BCyear n) = "B" ++ show n ++ "年"
  show (ACyear n) = "A" ++ show n ++ "年"

instance Show Century where
  show (BCcent cent w) = "B" ++ show cent ++ "C" ++ show w
  show (ACcent cent w) = "A" ++ show cent ++ "C" ++ show w

data Where = First | Half | Latter | Last
instance Show Where where
  show First = "前半"
  show Half = "中頃"
  show Latter = "後半"
  show Last = "末"

parseYear :: String -> Year
parseYear year = case year of
                      str@('B':_) -> BCyear $ read $ dropWhile isAlpha str
                      str@(x:_) -> ACyear $ read $ dropWhile isAlpha str

interc' :: Char -> String -> String
interc' c str = intercalate [c] (map return str)

showGraphicalYear :: Int -> String
showGraphicalYear y = intercalate "\n" $ (interc' ' ' $ show y) : ((take 3 resultDeluted) ++ ["----"] ++ (drop 3 $ take 6 resultDeluted) ++ ["----"] ++ (drop 6 resultDeluted))
  where
    ns :: [Int]
    ns = map ((read::String->Int) . (\x -> [x])) $ show y
    fs :: [Char -> String]
    fs = map replicate ns

    rs :: [String]
    rs = map ($ '□') fs

    water :: [String] -> String
    water strs = map (const ' ') $ mostLong strs

    dilute :: [String] -> [String]
    dilute strs = map (flip overwrite $ water strs) strs

    overwrite :: [a] -> [a] -> [a]
    overwrite [] [] = []
    overwrite [] bs = bs
    overwrite (a:as) (b:bs) = a:(overwrite as bs)

    whiteAndBlank :: [String]
    whiteAndBlank = dilute rs

    mostLong :: [String] -> String
    mostLong (s1:(s2:[])) | length s1 <= length s2 = s2
                          | length s1 > length s2 = s1
    mostLong (s1:(s2:ss)) | length s1 <= length s2 = mostLong $ s2:ss
                          | length s1 > length s2 = mostLong $ s1:ss

    result :: [String]
    result = map (interc' ' ') $ reverse $ transpose whiteAndBlank

    resultDeluted :: [String]
    resultDeluted = reverse $ overwrite (reverse result) (replicate 9 [' '])

toMelody :: Year -> [String]
toMelody (BCyear n) = map toTone $ show n
toMelody (ACyear n) = map toTone $ show n

-- represent eras by pitch names
toTone :: Char -> String
toTone '0' = "Cb"
toTone '1' = "C"
toTone '2' = "D"
toTone '3' = "E"
toTone '4' = "F"
toTone '5' = "G"
toTone '6' = "A"
toTone '7' = "B"
toTone '8' = "^C"
toTone '9' = "^D"

main :: IO ()
main = do
  years@(y:ys) <- getArgs
  let ys = map parseYear years
  putStrLn $ (show $ parseYear y) ++ "(" ++ (show $ year2cent $ parseYear y) ++ ")"
  putStrLn "-----\n"
  putStrLn $ showGraphicalYear $ read $ dropWhile isAlpha y
  putStrLn "-----"
  putStrLn $ intercalate " " $ toMelody $ parseYear y
