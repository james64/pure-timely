module Main where

import Prelude

import Affjax (Error, Request, Response, defaultRequest, printError)
import Affjax.Node (request)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, runExceptT)
import Data.Array ((!!), filter)
import Data.Array.NonEmpty (head, last)
import Data.Date (Date, adjust)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.HashMap (HashMap, fromArrayBy, lookup)
import Data.HTTP.Method (Method(..))
import Data.Int (fromString, round, toNumber)
import Data.List (fromFoldable)
import Data.List.Types (List, NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Base64 as B64
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
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
import Partial.Unsafe (unsafePartial)
import SalsitaRounding (Tag(..), TogglEntry(..), roundToQuarters, uniqueEntries, tagFromString, timelyMinutes)
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


togglFetch :: String -> String -> ExceptT String Aff String
togglFetch token url = ExceptT $ convert <$> request reqDef
  where
    reqDef :: Request String
    reqDef = defaultRequest
      { url = url
      , method = Left GET
      , headers = [
          RequestHeader "Content-Type" "application/json",
          RequestHeader "Authorization" ("Basic " <> (B64.encode (token <> ":api_token")))
        ]
      , responseFormat = RF.string
      }

    convert :: Either Error (Response String) -> Either String String
    convert (Left e)     = Left $ printError e
    convert (Right resp) = case resp.status of
      StatusCode 200 -> Right resp.body
      _              -> Left $ "StatusCode:" <> show resp.status <> " statusText: " <> resp.statusText <> " url: " <> url


fetchProjects :: String -> ExceptT String Aff String
fetchProjects token = togglFetch token "https://api.track.toggl.com/api/v9/workspaces/5744971/projects"


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
fetchEntries token date = togglFetch token url
  where
    toDateTime d = DateTime d bottom

    dash = Placeholder "-"

    formatter :: List FormatterCommand
    formatter = fromFoldable [YearFull, dash, MonthTwoDigits, dash, DayOfMonthTwoDigits]

    formatDate :: Date -> String
    formatDate d = format formatter (toDateTime d)

    dateStart :: String
    dateStart = formatDate date

    dateEnd' :: Maybe String
    dateEnd' = map formatDate $ adjust (Days 1.0) date

    dateEnd :: String
    dateEnd = fromMaybe "Data.Date.adjust_returns_maybe_facepalm" dateEnd'

    url :: String
    url = "https://api.track.toggl.com/api/v9/me/time_entries?start_date=" <> dateStart <> "&end_date=" <> dateEnd


type TogglEntryRawJson = { project_id::Int, duration::Int, description::String }
type TogglEntryJson = { project::String, duration::Int, description::String, tag::Tag }

descRegex :: Regex
descRegex = unsafePartial unsafeRe
  where
    reStr :: String
    reStr = "(\\[[a-zA-Z0-9]*\\]) (.*)"

    unsafeRe :: Partial => Regex
    unsafeRe = case (regex reStr noFlags) of
                 Right re -> re


unsafeFromMaybe :: Maybe String -> String
unsafeFromMaybe ms = unsafePartial $ pf ms
  where
    pf :: Partial => Maybe String -> String
    pf (Just s) = s


teFromRawJson :: HashMap Int String -> TogglEntryRawJson -> TogglEntryJson
teFromRawJson projMap rj =
  let
    descParsed :: { desc::String, tag::Tag }
    descParsed = case (match descRegex rj.description) of
                   Just (ne) -> {
                       desc: unsafeFromMaybe $ head ne,
                       tag: tagFromString $ unsafeFromMaybe $ last ne
                     }
                   Nothing   -> {
                       desc: rj.description,
                       tag: Devops
                     }

    toMinutes :: Int -> Int
    toMinutes seconds = round $ ((toNumber seconds) / 60.0)
  in
    {
      project: fromMaybe "UNKNOWN" (lookup rj.project_id projMap),
      duration: toMinutes rj.duration,
      description: descParsed.desc,
      tag: descParsed.tag
    }


toTogglEntries :: String -> HashMap Int String -> Either String (Array TogglEntry)
toTogglEntries jsonStr projMap =
  do
    rawJson <- mapLeft $ parsed
    pure $ map ((teFromRawJson projMap) >>> toEntry) rawJson
  where
    parsed :: Y.E (Array TogglEntryRawJson)
    parsed = Y.readJSON jsonStr

    toEntry :: TogglEntryJson -> TogglEntry
    toEntry tej = TogglEntry tej.project tej.description tej.tag tej.duration

    mapLeft :: Y.E (Array TogglEntryRawJson) -> Either String (Array TogglEntryRawJson)
    mapLeft (Left me) = Left $ renderErrors me
    mapLeft (Right a) = Right a

    renderErrors :: MultipleErrors -> String
    renderErrors me = fold $ map renderForeignError me


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
  let timelyEntries = filter (\e -> timelyMinutes e /= 0) $ roundToQuarters togUni
  except $ Right (togString <> "\n" <> show togEntries <> "\n" <> show togUni <> "\n" <> show timelyEntries)


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
