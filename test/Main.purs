module Test.Main where

import Prelude

import Data.Foldable (all, foldl)
import Data.HashMap (HashMap, empty, insertWith, toArrayBy)
import Data.Ord (abs)
import Data.Set (fromFoldable)
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Effect (Effect)
import Effect.Exception (Error, catchException, message)
import Effect.Class.Console (log)
import Node.Process (exit)
import SalsitaRounding (Tag, TimelyEntry(..), TogglEntry(..), roundToQuarters, timelyMinutes)
import Test.QuickCheck (Result, quickCheck, (<?>))

rounds15 :: Int -> Int -> Boolean
rounds15 from to = to `mod` 15 == 0 && abs (from - to) < 15

type TestFunc = Array TogglEntry -> Array TimelyEntry -> Result

onlyMinutesAreChanged :: TestFunc
onlyMinutesAreChanged togs tims =
  let
    dropMinsToggl  (TogglEntry  p r t _) = tuple3 p r t
    dropMinsTimely (TimelyEntry p r t _) = tuple3 p r t
    togglSet  = fromFoldable $ map dropMinsToggl togs
    timelySet = fromFoldable $ map dropMinsTimely tims

  in
    togglSet == timelySet <?> "Set of entries modified.\nInput: " <> show togs <> "\nOutput: " <> show tims

nonNegativeMinutes :: TestFunc
nonNegativeMinutes _ tims = all (\te -> 0 <= timelyMinutes te) tims <?> "Negative minutes found"

{-

- rounds15 sucetVsetkychPred sucetPo

- for V projekt: rounds15 sucetVsetkychVprojekte secetVprojektePo

- for V zaznamy: rounds15 pred a po

- Vsetky minuty nezaporne
-}

type TETuple   = Tuple3 String String Tag
type TEHashMap = HashMap TETuple Int

uniqueEntries :: Array TogglEntry -> Array TogglEntry
uniqueEntries togs = toArrayBy backToToggl $ foldl add empty togs
                     where
                       add :: TEHashMap -> TogglEntry -> TEHashMap
                       add hm (TogglEntry p n t m) = insertWith (\a b -> a+b) (tuple3 p n t) m hm

                       backToToggl :: TETuple -> Int -> TogglEntry
                       backToToggl t3 m = TogglEntry (get1 t3) (get2 t3) (get3 t3) m

errorHandler :: Error -> Effect Boolean
errorHandler e = map (\_ -> false) $ log $ message e


checkFuncAdapter :: TestFunc -> Array TogglEntry -> Result
checkFuncAdapter testF togs = testF uniqueIn output
                              where
                                uniqueIn = uniqueEntries togs
                                output   = roundToQuarters togs


runSingleTest :: TestFunc -> Effect Boolean
runSingleTest tf = catchException errorHandler do
                     quickCheck $ checkFuncAdapter tf
                     pure true


runTests :: Array TestFunc -> Effect Int
runTests ts = foldl run (pure 0) ts
              where
                run :: Effect Int -> TestFunc -> Effect Int
                run failCnt tf = do
                                   succ <- runSingleTest tf
                                   if succ then failCnt else (map (_+1) failCnt)

main :: Effect Unit
main = do
  failedCnt <- runTests ourTests
  exit failedCnt
  where
    ourTests :: Array TestFunc
    ourTests = [ onlyMinutesAreChanged
               , nonNegativeMinutes
               ]
