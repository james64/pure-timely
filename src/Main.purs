module Main where

import Prelude

import Affjax (Error, Request, Response, defaultRequest, printError)
import Affjax.Node (request)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, runExceptT)
import Data.Array ((!!))
import Data.Date (Date, adjust)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.HashMap (HashMap, fromArrayBy)
import Data.HTTP.Method (Method(..))
import Data.Int (fromString, toNumber)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDate)
import Foreign (MultipleErrors, renderForeignError)
import Node.Buffer as NB
import Node.Encoding (Encoding(UTF8))
import Node.ChildProcess as CP
import Node.Process (argv, exit)
import SalsitaRounding (TogglEntry, roundToQuarters, uniqueEntries)
import Yoga.JSON as Y

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
fetchProjects token = ExceptT $ (convert <$> request reqDef)
  where
    togglWorkspaceId = "5744971"

    reqDef :: Request String
    reqDef = defaultRequest
      { url = "https://api.track.toggl.com/api/v9/workspaces/" <> togglWorkspaceId <> "/projects"
      , method = Left GET
      , headers = [
          RequestHeader "Content-Type" "application/json"
        ]
      , withCredentials = true
      , username = Just token
      , password = Just "api_token"
      , responseFormat = RF.string
      }

    convert :: Either Error (Response String) -> Either String String
    convert (Left e)     = Left $ printError e
    convert (Right resp) = case resp.status of
      StatusCode 200 -> Right resp.body
      _              -> Left $ "StatusCode:" <> show resp.status <> " statusText: " <> resp.statusText


createProjMap :: String -> Either String (HashMap Int String)
createProjMap jsonStr =
  case parsed of
    Right p -> Right $ fromArrayBy _.id _.name p
    Left me -> Left $ err_ me
  where
    parsed :: Y.E (Array { id::Int, name::String })
    parsed = Y.readJSON jsonStr

    err_ :: MultipleErrors -> String
    err_ (NonEmptyList fe) = intercalate "\n" (map renderForeignError fe)


fetchEntries :: String -> Date -> ExceptT String Aff String
fetchEntries token date = do
  let date :: String
  let url :: String


-- curl -u $token:api_token -H 'Content-Type: application/json' 'https://api.track.toggl.com/api/v9/me/time_entries?start_date=2023-08-21&end_date=2023-08-22'

toTogglEntries :: String -> HashMap Int String -> Either String (Array TogglEntry)
toTogglEntries jsonStr projMap =

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
  projMap <- except $ createProjMap projJson
  togString <- fetchEntries togglTok targetDay
  togEntries <- except $ toTogglEntries togString projMap
  let togUni = uniqueEntries togEntries
  let timelyEntries = roundToQuarters togUni
  except $ Right (show timelyEntries)


logExit :: Int -> String -> Aff Unit
logExit code msg = liftEffect $ do
  log msg
  exit code


main :: Effect Unit
main = launchAff_ do
  res <- runExceptT program
  case res of
    Right r -> logExit 0 r
    Left e  -> logExit 1 e
