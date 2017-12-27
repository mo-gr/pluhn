module Main where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (catMaybes, head, mapWithIndex, reverse, tail)
import Data.Foldable (sum)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromMaybe)
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

luhnDouble :: Int -> Int
luhnDouble x = let xx = x * 2 in
  if xx > 9 then xx - 9
            else xx

digitize :: String -> Array Int
digitize str = catMaybes $ toDigit <$> toCharArray str

process :: Array Int -> Array Int
process is = mapWithIndex update is
             where update idx val = if even idx then val else luhnDouble val

luhnDigit :: Array Int -> Int
luhnDigit ds = mod (9 * (sum <<< reverse <<< fromMaybe empty <<< tail <<< reverse) ds) 10

isLuhn :: String -> Boolean
isLuhn input = let digits = digitize input in
  case (head <<< reverse) digits of
    Just head -> head == (luhnDigit $ (process) digits)
    _ -> false

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ show $ isLuhn valid
  log $ show $ isLuhn invalid
