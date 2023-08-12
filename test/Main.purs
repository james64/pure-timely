module Test.Main where

import Prelude

import Data.Ord (abs)
import Data.Set (fromFoldable)
import Data.Tuple.Nested (tuple3)
import Effect (Effect)
import Effect.Exception (Error, catchException, message)
import Effect.Class.Console (log)
import SalsitaRounding (TogglEntry(..), TimelyEntry(..), roundToQuarters)
import Test.QuickCheck (quickCheck)

rounds15 :: Int -> Int -> Boolean
rounds15 from to = to `mod` 15 == 0 && abs (from - to) < 15

onlyMinutesAreChanged :: Array TogglEntry -> Array TimelyEntry -> Boolean
onlyMinutesAreChanged togs tims =
  let
    dropMinsToggl  (TogglEntry  p r t _) = tuple3 p r t
    dropMinsTimely (TimelyEntry p r t _) = tuple3 p r t
    togglSet  = fromFoldable $ map dropMinsToggl togs
    timelySet = fromFoldable $ map dropMinsTimely tims
  in
    togglSet == timelySet



{-

- rounds15 sucetVsetkychPred sucetPo

- for V projekt: rounds15 sucetVsetkychVprojekte secetVprojektePo

- for V zaznamy: rounds15 pred a po

- Vsetky minuty nezaporne
-}

runTest :: Array TogglEntry -> Boolean
runTest tes = onlyMinutesAreChanged tes $ roundToQuarters tes

errorHandler :: Error -> Effect Unit
errorHandler = log <<< message

main :: Effect Unit
main = catchException errorHandler do
  quickCheck runTest
