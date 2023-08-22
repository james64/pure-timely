module SalsitaRounding where

import Prelude

import Data.Array (drop, filter, groupBy, sortWith, take)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldr, sum)
import Data.Hashable (class Hashable)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, elements)

data Tag = Devops | Meeting | Research | Review | Other

derive instance Eq Tag
derive instance Ord Tag

instance Show Tag where
  show Devops   = "Devops"
  show Meeting  = "Meeting"
  show Research = "Research"
  show Review   = "Review"
  show Other    = "Other"

instance Arbitrary Tag where
  arbitrary = arbitraryFrom [ Devops , Meeting , Research , Review , Other ]

instance Hashable Tag where
  hash Devops   = 0
  hash Meeting  = 1
  hash Research = 2
  hash Review   = 3
  hash Other    = 4


arbitraryFrom :: forall (a :: Type). Array a -> Gen a
arbitraryFrom as = elements $ unsafePartial fromJust $ NEA.fromArray as

arbitraryProject :: Gen String
arbitraryProject = arbitraryFrom [
                     "Bank Of Mongolia",
                     "Sterile Corporate",
                     "Failing Startup",
                     "MyCompanyTM" ]

arbitraryMessage :: Gen String
arbitraryMessage = arbitraryFrom [
                     "desktop refactoring",
                     "sleeping at meeting",
                     "pr review",
                     "windows reinstall",
                     "lunaform debugging",
                     "outsourcing",
                     "copying and pasting from stack overflow",
                     "doning nothing" ]

data TogglEntry = TogglEntry String String Tag Int

derive instance Eq  TogglEntry
derive instance Ord TogglEntry

togglProject :: TogglEntry -> String
togglProject (TogglEntry p _ _ _) = p

togglName :: TogglEntry -> String
togglName (TogglEntry _ n _ _) = n

togglTag :: TogglEntry -> Tag
togglTag (TogglEntry _ _ t _) = t

togglMinutes :: TogglEntry -> Int
togglMinutes (TogglEntry _ _ _ m) = m

showEntry :: String -> String -> Tag -> Int -> String
showEntry p m t d = show p <> " " <> show m <> " " <> show t <> " " <> show d

instance Show TogglEntry where
  show (TogglEntry p m t d) = "TogglEntry " <> showEntry p m t d

instance Arbitrary TogglEntry where
  arbitrary = TogglEntry <$> arbitraryProject <*> arbitraryMessage <*> arbitrary <*> chooseInt 1 250



data TimelyEntry = TimelyEntry String String Tag Int

derive instance Eq  TimelyEntry
derive instance Ord TimelyEntry

timelyProject :: TimelyEntry -> String
timelyProject (TimelyEntry p _ _ _) = p

timelyName :: TimelyEntry -> String
timelyName (TimelyEntry _ n _ _) = n

timelyTag :: TimelyEntry -> Tag
timelyTag (TimelyEntry _ _ t _) = t

timelyMinutes :: TimelyEntry -> Int
timelyMinutes (TimelyEntry _ _ _ m) = m

instance Show TimelyEntry where
  show (TimelyEntry p m t d) = "TimelyEntry " <> showEntry p m t d

------

roundTo15 :: Int -> { mins::Int, diff::Int }
roundTo15 i = case (i `mod` 15) of
                r | r <= 7 -> { mins: i - r,      diff: (-r)   }
                r          -> { mins: i - r + 15, diff: (15-r) }

updateItems' :: forall a. Int -> (Int -> Int) -> (Int -> Int) -> Array { result::{ mins::Int, diff::Int }, tag::a } -> Array { mins::Int, tag::a }
updateItems' cnt sortProj update items =
  let
    reshape e        = { mins: e.result.mins, tag: e.tag }
    sortProjRec r    = sortProj r.result.diff
    mostChangedFirst = map reshape $ sortWith sortProjRec items
    mapHelper r      = { mins: update r.mins, tag: r.tag }
  in
    (map mapHelper $ take cnt mostChangedFirst) <> drop cnt mostChangedFirst


updateItems :: forall a. Int -> Array { result::{ mins::Int, diff::Int }, tag::a } -> Array { mins::Int, tag::a }
updateItems adj items = if adj >= 0 then
                          updateItems' adj (\i -> i) (_+15) items
                        else
                          updateItems' (-adj) (\i -> (-i)) (_-15) items


-- round minutes to nearest multiple of 15
-- make them sum to global goal by rounding some of the minutes to second nearest multiple of 15
spread :: forall a. Int -> Array { mins::Int, tag::a } -> Array { mins::Int, tag::a }
spread glob items =
  let
    target :: Int
    target = (roundTo15 glob).mins

    mapFunc :: { mins::Int, tag::a } -> { result::{ mins::Int, diff::Int }, tag::a }
    mapFunc record = { result: roundTo15 record.mins, tag: record.tag }

    firstRound :: Array { result::{ mins::Int, diff::Int }, tag::a }
    firstRound = map mapFunc items

    firstSum = sum $ map (_.result.mins) firstRound

    adjustment = (target - firstSum) `div` 15
  in
    updateItems adjustment firstRound


roundToQuarters :: Array TogglEntry -> Array TimelyEntry
roundToQuarters togs =
  let
    globalSum :: Int
    globalSum = foldr (+) 0 $ map togglMinutes togs

    h1 :: Array TogglEntry
    h1 = sortWith togglProject togs

    h2 :: Array (NEA.NonEmptyArray TogglEntry)
    h2 = groupBy (\a b -> togglProject a == togglProject b) h1

    f2 :: NEA.NonEmptyArray TogglEntry -> Int
    f2 = f3 >>> sum

    f3 :: NEA.NonEmptyArray TogglEntry -> NEA.NonEmptyArray Int
    f3 = map togglMinutes

    f1 :: NEA.NonEmptyArray TogglEntry -> { project::String, minutes::Int }
    f1 a = { project: (togglProject $ NEA.head a), minutes: (f2 a) }

    projectSums :: Array { project::String, minutes::Int }
    projectSums = map f1 h2

    projSumsRounded :: Array { mins::Int, tag::String }
    projSumsRounded = spread globalSum $ map (\r -> { mins: r.minutes, tag: r.project }) projectSums

    -- join target sum and toggl entries for given project
    joinForProj :: { mins::Int, tag::String } -> { mins::Int, togs::Array { mins::Int, tag::TogglEntry} }
    joinForProj r = let
                      projOnly = filter (\e -> togglProject e == r.tag) togs
                    in
                      {
                        mins: r.mins,
                        togs: map (\e -> { mins: togglMinutes e, tag: e}) projOnly
                      }

    roundedItems :: Array { mins::Int, tag::TogglEntry }
    roundedItems = do
                     projMinutes <- projSumsRounded
                     let wpTogs = joinForProj projMinutes
                     spread wpTogs.mins wpTogs.togs
  in
    map (\r -> TimelyEntry (togglProject r.tag) (togglName r.tag) (togglTag r.tag) r.mins) roundedItems
