module Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!), head)
import Data.Date (Date, adjust)
import Data.Either (Either(..), note)
import Data.HashMap (HashMap, empty)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDate)
import Node.Process (argv, exit)
import SalsitaRounding (TogglEntry(..), roundToQuarters, uniqueEntries)

getArgument :: ExceptT String Effect Int
getArgument = ExceptT do
  arr <- argv
  case (arr !! 2) of
    Nothing -> pure $ Left "No arguments passed"
    Just h  -> case (fromString h) of
                 Nothing -> pure $ Left ("Not a number: " <> show h)
                 Just n  -> pure $ Right n

targetDate :: Int -> Date -> Either String Date
targetDate diff today = conv $ adjust (Days $ toNumber diff) today
  where
    conv Nothing  = Left "No target date"
    conv (Just d) = Right d

togglToken :: ExceptT String Effect String
togglToken = except (Left "togglToken")

fetchProjects :: String -> ExceptT String Effect String
fetchProjects token = except (Left "fetchProjects")

createProjMap :: String -> HashMap Int String
createProjMap json = empty

fetchEntries :: String -> ExceptT String Effect String
fetchEntries token = except (Left "fetchEntries")

toTogglEntries :: String -> Either String (Array TogglEntry)
toTogglEntries json = Left "toTogglEntries"

program :: ExceptT String Effect String
program = do
  dayOffset <- getArgument
  today <- ExceptT $ map Right nowDate
  targetDay <- except $ targetDate dayOffset today
  togglTok <- togglToken
  projJson <- fetchProjects togglTok
  let projMap = createProjMap projJson
  togString <- fetchEntries togglTok
  togEntries <- except $ toTogglEntries togString
  let togUni = uniqueEntries togEntries
  let timelyEntries = roundToQuarters togUni
  except $ Right (show timelyEntries)

main :: Effect Unit
main = do
  res <- runExceptT program
  case res of
    Right r -> log r >>= (\_ -> pure unit)
    Left e  -> log e >>= (\_ -> exit 1)
