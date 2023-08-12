module SalsitaRounding where

import Prelude

import Data.Array.NonEmpty as NEA
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

instance Show TogglEntry where
  show (TogglEntry project msg tag dur) =
    "TogglEntry " <> show project <> " " <> show msg <> " " <> show tag <> " " <> show dur

instance Arbitrary TogglEntry where
  arbitrary = TogglEntry <$> arbitraryProject <*> arbitraryMessage <*> arbitrary <*> chooseInt 1 250



data TimelyEntry = TimelyEntry String String Tag Int

roundToQuarters :: Array TogglEntry -> Array TimelyEntry
roundToQuarters _ = []
