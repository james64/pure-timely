module Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!), head)
import Data.Date (Date, adjust)
import Data.Either (Either(..), note)
import Data.HashMap (HashMap, empty)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDate)
import Node.Buffer as NB
import Node.Encoding (Encoding(UTF8))
import Node.ChildProcess as CP
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

togglToken :: Effect String
togglToken = do
  buff <- CP.execFileSync "pass" ["jad/toggl/token"] CP.defaultExecSyncOptions
  NB.toString UTF8 buff

fetchProjects :: String -> ExceptT String Aff String
fetchProjects token = except (Left "fetchProjects")

createProjMap :: String -> HashMap Int String
createProjMap json = empty

fetchEntries :: String -> ExceptT String Aff String
fetchEntries token = except (Left "fetchEntries")

toTogglEntries :: String -> Either String (Array TogglEntry)
toTogglEntries json = Left "toTogglEntries"

getTargetDay :: ExceptT String Aff Date
getTargetDay = mapExceptT liftEffect getEffect
    where
      getEffect :: ExceptT String Effect Date
      getEffect = do
        dayOffset <- getArgument
        today <- ExceptT $ map Right nowDate
        except $ targetDate dayOffset today

program :: ExceptT String Aff String
program = do
  targetDay <- getTargetDay
  togglTok <- ExceptT $ map Right (liftEffect togglToken)
  projJson <- fetchProjects togglTok
  let projMap = createProjMap projJson
  togString <- fetchEntries togglTok
  togEntries <- except $ toTogglEntries togString
  let togUni = uniqueEntries togEntries
  let timelyEntries = roundToQuarters togUni
  except $ Right (show timelyEntries)


affLog :: String -> Aff Unit
affLog = liftEffect <<< log


main :: Effect Unit
main = launchAff_ do
  res <- runExceptT program
  case res of
    Right r -> affLog r >>= (\_ -> pure unit)
    Left e  -> affLog e >>= (\_ -> liftEffect $ exit 1)
