module Test.Main where

import Prelude

import Data.Array (sort)
import Data.Foldable (all, foldr, foldl, sum)
import Data.HashMap (HashMap, empty, insertWith, toArrayBy)
import Data.Ord (abs)
import Data.Set (fromFoldable)
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Effect (Effect)
import Effect.Exception (Error, catchException, message)
import Effect.Class.Console (log)
import Node.Process (exit)
import SalsitaRounding (Tag, TimelyEntry(..), TogglEntry(..), roundToQuarters, togglMinutes, timelyMinutes)
import Test.QuickCheck (Result, quickCheck, (<?>))

rounds15 :: Int -> Int -> Boolean
rounds15 from to = to `mod` 15 == 0 && abs (from - to) < 15

type TestFunc  = Array TogglEntry -> Array TimelyEntry -> Result
type NamedTest = { tf::TestFunc, name::String }

namedTest :: TestFunc -> String -> NamedTest
namedTest tf name = { tf, name }

showA :: forall (a :: Type). Show a => Ord a => Array a -> String
showA arr = "[" <> (foldl cf "" $ map show $ sort arr) <> "\n]"
            where
              cf :: String -> String -> String
              cf acc e = acc <> "\n  " <> e

showInputOutput :: Array TogglEntry -> Array TimelyEntry -> String
showInputOutput togs tims = "Input: " <> showA togs <> "\nOutput: " <> showA tims

onlyMinutesAreChanged :: TestFunc
onlyMinutesAreChanged togs tims =
  let
    dropMinsToggl  (TogglEntry  p r t _) = tuple3 p r t
    dropMinsTimely (TimelyEntry p r t _) = tuple3 p r t
    togglSet  = fromFoldable $ map dropMinsToggl togs
    timelySet = fromFoldable $ map dropMinsTimely tims
  in
    togglSet == timelySet <?> showInputOutput togs tims

nonNegativeMinutes :: TestFunc
nonNegativeMinutes _ tims = all (\te -> 0 <= timelyMinutes te) tims <?> "Negative minutes found"

totalSumRounds :: TestFunc
totalSumRounds togs tims = rounds15 togglSum timelySum <?> showInputOutput togs tims
                           where
                             togglSum  = sum $ map togglMinutes togs
                             timelySum = sum $ map timelyMinutes tims

{-
- for V projekt: rounds15 sucetVsetkychVprojekte secetVprojektePo

- for V zaznamy: rounds15 pred a po
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


runSingleTest :: TestFunc -> String -> Effect Boolean
runSingleTest tf name = catchException errorHandler do
                     log $ "\n===== " <> name <> " =====\n"
                     quickCheck $ checkFuncAdapter tf
                     pure true


runTests :: Array NamedTest -> Effect Int
runTests ts = foldr run (pure 0) ts
              where
                run :: NamedTest -> Effect Int -> Effect Int
                run nt failCnt = do
                  succ <- runSingleTest nt.tf nt.name
                  if succ then failCnt else (map (_+1) failCnt)


main :: Effect Unit
main = do
  failedCnt <- runTests ourTests
  exit failedCnt
  where
    ourTests :: Array NamedTest
    ourTests = [ namedTest onlyMinutesAreChanged "Only minutes are changed"
               , namedTest nonNegativeMinutes "No negative minutes after change"
               , namedTest totalSumRounds "Total sum rounds ok"
               ]
