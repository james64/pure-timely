module SalsitaRounding where

import Prelude

import Data.Array.NonEmpty as NEA
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

roundToQuarters :: Array TogglEntry -> Array TimelyEntry
roundToQuarters _ = []
