module Test.Main where

import Prelude

import Data.Array (groupBy, sort, sortWith, toUnfoldable, zip)
import Data.Array.NonEmpty as NEA
import Data.Foldable (all, fold, foldr, foldl, sum)
import Data.HashMap (HashMap, empty, insertWith, toArrayBy)
import Data.List (List(..), (:))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Effect (Effect)
import Effect.Exception (catchException, message)
import Effect.Class.Console (log)
import Node.Process (exit)
import SalsitaRounding (
  Tag,
  TimelyEntry(..),
  TogglEntry(..),
  roundToQuarters,
  togglMinutes,
  togglProject,
  timelyMinutes,
  timelyProject
)
import Test.QuickCheck (Result(..), quickCheck, (<?>))

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

nonNegativeMinutes :: TestFunc
nonNegativeMinutes _ tims = all (\te -> 0 <= timelyMinutes te) tims <?> "Negative minutes found"

totalSumRounds :: TestFunc
totalSumRounds togs tims = rounds15 togglSum timelySum <?> failMsg
                           where
                             togglSum  = sum $ map togglMinutes togs
                             timelySum = sum $ map timelyMinutes tims
                             failMsg   = fold [
                                           "TogglSum: ",
                                           show togglSum,
                                           "\nTimelySum: ",
                                           show timelySum
                                         ]

sameEntry :: TogglEntry -> TimelyEntry -> Boolean
sameEntry (TogglEntry p1 n1 t1 _) (TimelyEntry p2 n2 t2 _) = p1 == p2 && n1 == n2 && t1 == t2


minsCompareSorted :: List TogglEntry -> List TimelyEntry -> Result
minsCompareSorted Nil      Nil    = Success
minsCompareSorted (to:_)   Nil    = false <?> "Un-matched toggl entry: " <> show to
minsCompareSorted Nil      (ti:_) = false <?> "Un-matched timely entry: " <> show ti
minsCompareSorted (to:tos) (ti:tis)
  | not $ sameEntry to ti                               = false <?> "Un-matched entries:\n" <> show to <> "\n" <> show ti
  | not $ rounds15 (togglMinutes to) (timelyMinutes ti) = false <?> "Entry mis-rounded:\n" <> show to <> "\n" <> show ti
  | otherwise                                           = minsCompareSorted tos tis

minutesRounds :: TestFunc
minutesRounds togs tims = minsCompareSorted togglList timelyList
                          where
                            togglList  = toUnfoldable $ sortWith togsProj togs
                            timelyList = toUnfoldable $ sortWith timsProj tims

                            togsProj (TogglEntry  project name tag _) = {project, name, tag}
                            timsProj (TimelyEntry project name tag _) = {project, name, tag}

projectSumCheck :: Tuple (NEA.NonEmptyArray TogglEntry) (NEA.NonEmptyArray TimelyEntry) -> Result
projectSumCheck (Tuple neTogs neTims) = rounds15 togSum timSum <?> failMsg
  where
    togSum  = sum $ map togglMinutes neTogs
    timSum  = sum $ map timelyMinutes neTims
    project = show $ togglProject $ NEA.head neTogs
    failMsg = fold [
                "Sums for ",
                project,
                " do not match.\ntogSum: ",
                show togSum,
                "\ntimSum: ",
                show timSum
              ]

minutesRoundsPerProject :: TestFunc
minutesRoundsPerProject togs tims =
  let
    togGroups = groupBy (\a b -> togglProject  a == togglProject  b) $ sortWith togglProject  togs
    timGroups = groupBy (\a b -> timelyProject a == timelyProject b) $ sortWith timelyProject tims

    check' :: List (Tuple (NEA.NonEmptyArray TogglEntry) (NEA.NonEmptyArray TimelyEntry)) -> Result
    check' Nil    = Success
    check' (p:ps) = case projectSumCheck p of
                     Success      -> check' ps
                     f@(Failed _) -> f
  in
    check' $ toUnfoldable $ zip togGroups timGroups


type TETuple   = Tuple3 String String Tag
type TEHashMap = HashMap TETuple Int

uniqueEntries :: Array TogglEntry -> Array TogglEntry
uniqueEntries togs = toArrayBy backToToggl $ foldl add empty togs
                     where
                       add :: TEHashMap -> TogglEntry -> TEHashMap
                       add hm (TogglEntry p n t m) = insertWith (\a b -> a+b) (tuple3 p n t) m hm

                       backToToggl :: TETuple -> Int -> TogglEntry
                       backToToggl t3 m = TogglEntry (get1 t3) (get2 t3) (get3 t3) m


showInputOutput :: Array TogglEntry -> Array TimelyEntry -> String
showInputOutput togs tims = "Input: " <> showA togs <> "\nOutput: " <> showA tims

checkFuncAdapter :: TestFunc -> Array TogglEntry -> Result
checkFuncAdapter testF togs = case (testF uniqueIn output) of
                                Success    -> Success
                                Failed msg -> false <?> msg <> "\nin\n" <> showInputOutput togs output
                              where
                                uniqueIn = uniqueEntries togs
                                output   = roundToQuarters uniqueIn


runSingleTest :: TestFunc -> Effect Result
runSingleTest tf = catchException (message >>> Failed >>> pure) do
                     quickCheck $ checkFuncAdapter tf
                     pure Success


runTests :: Array NamedTest -> Effect Int
runTests ts = foldr run (pure 0) ts
              where
                run :: NamedTest -> Effect Int -> Effect Int
                run nt failCnt = do
                  log $ "\n===== " <> nt.name <> " =====\n"
                  result <- runSingleTest nt.tf
                  case result of
                    Success  -> failCnt
                    Failed m -> do
                      log m
                      map (_+1) failCnt


main :: Effect Unit
main = do
  failedCnt <- runTests ourTests
  exit failedCnt
  where
    ourTests :: Array NamedTest
    ourTests = [ namedTest nonNegativeMinutes "No negative minutes after change"
               , namedTest totalSumRounds "Total sum rounds ok"
               , namedTest minutesRounds "Closest rounding of minutes"
               , namedTest minutesRoundsPerProject "Closest rounding per project"
               ]
