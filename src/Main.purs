module Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!), head)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.HashMap (HashMap, empty)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
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


getToday :: Effect (Either String Date)
getToday = pure (Left "getToday")

targetDate :: Int -> Date -> Date
targetDate diff today = today

togglToken :: Effect (Either String String)
togglToken = pure (Left "togglToken")

fetchProjects :: String -> Effect (Either String String)
fetchProjects token = pure (Left "fetchProjects")

createProjMap :: String -> HashMap Int String
createProjMap json = empty

fetchEntries :: String -> Effect (Either String String)
fetchEntries token = pure (Left "fetchEntries")

toTogglEntries :: String -> Either String (Array TogglEntry)
toTogglEntries json = Left "toTogglEntries"

program :: ExceptT String Effect Int
program = getArgument

--   today <- getToday
--   targetDay = targetDate dayArg today
--   togglTok <- togglToken
--   projJson <- fetchProjects togglTok
--   projMap = createProjMap projJson
--   togString <- fetchEntries togglTok
--   togEntries <- toTogglEntries
--   togUni = uniqueEntries togEntries
--   timelyEntries = roundToQuarters togUni
--   log timelyEntries

main :: Effect Unit
main = do
  res <- runExceptT program
  case res of
    Right _ -> log "ok" >>= (\_ -> pure unit)
    Left e  -> log e    >>= (\_ -> exit 1)
