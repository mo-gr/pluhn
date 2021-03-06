module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (catMaybes, mapWithIndex)
import Data.Foldable (sum)
import Data.Int (even)
import Data.Maybe (Maybe(Nothing))
import Data.String (toCharArray)

valid :: String
valid = "79927398713"
invalid :: String
invalid = "79927398710"

toDigit :: Char -> Maybe Int
toDigit '0' = pure 0
toDigit '1' = pure 1
toDigit '2' = pure 2
toDigit '3' = pure 3
toDigit '4' = pure 4
toDigit '5' = pure 5
toDigit '6' = pure 6
toDigit '7' = pure 7
toDigit '8' = pure 8
toDigit '9' = pure 9
toDigit _   = Nothing

digitize :: String -> Array Int
digitize str = catMaybes $ toDigit <$> toCharArray str

luhnDouble :: Int -> Int
luhnDouble x = if xx > 9
               then xx - 9
               else xx
               where xx = 2 * x

process :: Array Int -> Array Int
process is = mapWithIndex update is
             where update idx val = if even idx
                                    then val
                                    else luhnDouble val

unitDigit :: Int -> Int
unitDigit x = mod x 10

isLuhn :: String -> Boolean
isLuhn = eq 0
            <<< unitDigit
            <<< sum
            <<< process
            <<< digitize

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Valid: " <> valid
  logShow $ isLuhn valid
  log $ "Invalid: " <> invalid
  logShow $ isLuhn invalid
