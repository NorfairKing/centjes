{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Talking to Stripe.
--
-- The only module in this package that does IO, so that everything which decides what
-- a VAT return says is a pure function over data a test can hold.
--
-- Reports are asynchronous: ask for one over an interval, wait for Stripe to compute
-- it, then fetch a CSV from a different host.  The columns are always named in the
-- request, so the CSV that comes back has a shape this importer chose rather than one
-- it has to discover.
module Centjes.Stripe.API
  ( StripeKey (..),
    ReportRunId (..),
    ReportRunStatus (..),
    ReportRun (..),
    StripeList (..),
    StripeM,
    StripeApiError (..),
    renderStripeApiError,
    fetchReportType,
    fetchReport,
    fetchPayouts,
    PollStep (..),
    maximumPolls,
    pollStep,
    PageStep (..),
    maximumPages,
    pageStep,
  )
where

import Autodocodec
import Centjes.Stripe.Payout
import Centjes.Stripe.Report
import Centjes.Stripe.Timestamp
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Validity
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP

-- | A Stripe API key.
--
-- Read access to the reports, to payouts and to the balance is all this importer
-- needs, plus whatever permission lets it create a report run.  Creating a report run
-- is the only write it ever does.
--
-- Deliberately has no 'Show' instance, so that a key cannot reach a log line or a
-- crash report by accident.  A key that does has to be rotated.
newtype StripeKey = StripeKey {unStripeKey :: Text}

data StripeApiError
  = StripeApiErrorHttp !String !HttpException
  | StripeApiErrorStatus !String !Int !LB.ByteString
  | StripeApiErrorDecode !String !String
  | StripeApiErrorReportFailed !ReportTypeId !ReportRunId
  | StripeApiErrorReportNoUrl !ReportTypeId !ReportRunId
  | StripeApiErrorReportTimedOut !ReportTypeId !ReportRunId !Int
  | StripeApiErrorPagingStuck !String !Text
  | StripeApiErrorTooManyPages !String !Int

renderStripeApiError :: StripeApiError -> Text
renderStripeApiError =
  Text.pack . \case
    -- Showing an 'HttpException' shows the request that failed, headers and all.  That is
    -- only safe because http-client's own 'Show' redacts the Authorization header, which is
    -- where the key lives.  Anything that formats the request itself has to redact it.
    StripeApiErrorHttp url e -> unwords ["The request to", url, "failed:", show e]
    StripeApiErrorStatus url status body ->
      unlines
        [ unwords ["The request to", url, "returned status", show status <> ":"],
          Text.unpack (TE.decodeUtf8Lenient (LB.toStrict body))
        ]
    StripeApiErrorDecode url reason ->
      unlines
        [ unwords ["The response from", url, "is not what this importer expects:"],
          reason
        ]
    StripeApiErrorReportFailed reportTypeId' runId ->
      unwords
        [ "Stripe could not produce the report",
          show (unReportTypeId reportTypeId'),
          "(run",
          Text.unpack (unReportRunId runId) <> ")."
        ]
    StripeApiErrorReportNoUrl reportTypeId' runId ->
      unwords
        [ "Stripe finished the report",
          show (unReportTypeId reportTypeId'),
          "(run",
          Text.unpack (unReportRunId runId) <> ")",
          "without saying where to fetch it."
        ]
    StripeApiErrorReportTimedOut reportTypeId' runId polls ->
      unlines
        [ unwords
            [ "The report",
              show (unReportTypeId reportTypeId'),
              "(run",
              Text.unpack (unReportRunId runId) <> ")",
              "was still not ready after",
              show polls,
              "checks."
            ],
          "Reports usually take a minute or two.  Running the importer again starts over from here."
        ]
    StripeApiErrorPagingStuck url cursor ->
      unlines
        [ unwords ["The listing at", url, "says there is more after", show cursor <> ","],
          "but that is the cursor it was already asked for, so asking again would fetch the same page forever."
        ]
    StripeApiErrorTooManyPages url limit ->
      unwords
        [ "The listing at",
          url,
          "still had more after",
          show limit,
          "pages, which is where this importer stops rather than page on without end."
        ]

-- | Talking to Stripe, with whatever went wrong handed back rather than printed.
--
-- One error ends the run, so this is an 'ExceptT' rather than accumulating: there is no
-- second thing to learn from a request that did not happen.
type StripeM = ExceptT StripeApiError (LoggingT IO)

-- | A runaway backstop on how many pages to follow.
maximumPages :: Int
maximumPages = 10_000

-- | How many times to ask whether a report is ready before giving up.
--
-- At six seconds apart that is five minutes, well past the minute or two Stripe says a
-- report takes, and giving up is harmless: the next run asks again.
maximumPolls :: Int
maximumPolls = 50

-- | What to do once a page of a listing has been yielded.
data PageStep
  = PageStepStop
  | PageStepContinue !Text
  | PageStepStuck !Text
  | PageStepTooManyPages !Int
  deriving stock (Show, Eq)

-- | Decide how a listing continues after a page.
--
-- Pure, and separate from the request that fetched the page, because every way paging
-- can go wrong is a way this decision can go wrong, and a fake HTTP server is not a
-- thing to test against.
pageStep :: Int -> Maybe Text -> Bool -> Maybe Text -> PageStep
pageStep pagesFetched askedWith hasMore mLastCursor
  | not hasMore = PageStepStop
  | pagesFetched >= maximumPages = PageStepTooManyPages maximumPages
  | otherwise = case mLastCursor of
      Nothing -> PageStepStop
      Just cursor
        | Just cursor == askedWith -> PageStepStuck cursor
        | otherwise -> PageStepContinue cursor

-- | What to do after asking whether a report is ready.
data PollStep
  = -- | Fetch the CSV from here.
    PollStepReady !Text
  | PollStepWait
  | PollStepFailed
  | -- | Finished, but with nowhere to fetch it from.
    PollStepNoUrl
  | PollStepTimedOut !Int
  deriving stock (Show, Eq)

-- | Decide what to do with a report run that was just looked at.
--
-- Pure for the same reason as 'pageStep': waiting on someone else's asynchronous job
-- has a handful of outcomes and each of them wants a test.
pollStep :: Int -> ReportRun -> PollStep
pollStep pollsSoFar run = case reportRunStatus run of
  ReportRunStatusFailed -> PollStepFailed
  ReportRunStatusSucceeded -> case reportRunResultUrl run of
    Nothing -> PollStepNoUrl
    Just url -> PollStepReady url
  ReportRunStatusPending
    | pollsSoFar >= maximumPolls -> PollStepTimedOut maximumPolls
    | otherwise -> PollStepWait

-- | What Stripe says about a kind of report, including how much of it is computed.
{-# ANN fetchReportType ("NOCOVER" :: String) #-}
fetchReportType :: Manager -> StripeKey -> ReportTypeId -> StripeM ReportType
fetchReportType man key reportTypeId' =
  requireApi $
    getJSON
      man
      key
      ("https://api.stripe.com/v1/reporting/report_types/" <> Text.unpack (unReportTypeId reportTypeId'))
      []

-- | Ask for a report over an interval, wait for it, and read it.
--
-- The interval is half-open, as Stripe defines it: from @start@ up to but not
-- including @end@.  The columns are named in the request so that a name Stripe has
-- changed fails here, with Stripe saying which, rather than further along where a
-- column of numbers might be read as something else.
{-# ANN fetchReport ("NOCOVER" :: String) #-}
fetchReport ::
  Manager ->
  StripeKey ->
  ReportTypeId ->
  -- | Interval start, inclusive.
  StripeTimestamp ->
  -- | Interval end, exclusive.
  StripeTimestamp ->
  [ReportColumn] ->
  -- | Extra parameters, such as which currency to report in.
  [(ByteString, ByteString)] ->
  -- | The CSV exactly as Stripe sent it.
  --
  -- Returned unparsed because the caller saves these bytes as the evidence for the
  -- month it computes from them, and reads them back on a later run.  Saving what
  -- was read, rather than re-rendering it, is what makes the attached file and the
  -- booked figures incapable of disagreeing.
  StripeM LB.ByteString
fetchReport man key reportTypeId' start end columns extraParameters = do
  logInfoN $
    Text.pack $
      unwords
        [ "Asking Stripe for",
          Text.unpack (unReportTypeId reportTypeId'),
          "from",
          show (stripeTimestampDay start),
          "up to",
          show (stripeTimestampDay end)
        ]
  run <-
    requireApi $
      postJSON
        man
        key
        "https://api.stripe.com/v1/reporting/report_runs"
        ( concat
            [ [("report_type", TE.encodeUtf8 (unReportTypeId reportTypeId'))],
              [("parameters[interval_start]", numeric (unStripeTimestamp start))],
              [("parameters[interval_end]", numeric (unStripeTimestamp end))],
              [ ( TE.encodeUtf8 (Text.pack ("parameters[columns][" <> show i <> "]")),
                  TE.encodeUtf8 (unReportColumn column)
                )
              | (i, column) <- zip [0 :: Int ..] columns
              ],
              map (\(k, v) -> ("parameters[" <> k <> "]", v)) extraParameters
            ]
        )
  url <- waitForReport man key reportTypeId' 0 run
  requireApi $ getRaw man key (Text.unpack url)
  where
    numeric = TE.encodeUtf8 . Text.pack . show

{-# ANN waitForReport ("NOCOVER" :: String) #-}
waitForReport :: Manager -> StripeKey -> ReportTypeId -> Int -> ReportRun -> StripeM Text
waitForReport man key reportTypeId' pollsSoFar run = do
  let runId = reportRunId run
  case pollStep pollsSoFar run of
    PollStepReady url -> pure url
    PollStepFailed -> throwE $ StripeApiErrorReportFailed reportTypeId' runId
    PollStepNoUrl -> throwE $ StripeApiErrorReportNoUrl reportTypeId' runId
    PollStepTimedOut polls -> throwE $ StripeApiErrorReportTimedOut reportTypeId' runId polls
    PollStepWait -> do
      liftIO $ threadDelay 6_000_000
      run' <-
        requireApi $
          getJSON
            man
            key
            ("https://api.stripe.com/v1/reporting/report_runs/" <> Text.unpack (unReportRunId runId))
            []
      waitForReport man key reportTypeId' (succ pollsSoFar) run'

-- | Every payout whose money left the balance within an interval.
{-# ANN fetchPayouts ("NOCOVER" :: String) #-}
fetchPayouts :: Manager -> StripeKey -> StripeTimestamp -> StripeTimestamp -> StripeM [Payout]
fetchPayouts man key start end = go 0 Nothing []
  where
    url = "https://api.stripe.com/v1/payouts"
    numeric = TE.encodeUtf8 . Text.pack . show
    go pagesFetched mStartingAfter acc = do
      let query =
            concat
              [ [("limit", "100")],
                [("created[gte]", numeric (unStripeTimestamp start))],
                [("created[lt]", numeric (unStripeTimestamp end))],
                [("starting_after", TE.encodeUtf8 cursor) | cursor <- maybeToList mStartingAfter]
              ]
      list <- requireApi $ getJSON man key url query
      let payouts = stripeListData list
      let mLastCursor = case reverse payouts of
            [] -> Nothing
            (lastPayout : _) -> Just (unPayoutId (payoutId lastPayout))
      case pageStep (succ pagesFetched) mStartingAfter (stripeListHasMore list) mLastCursor of
        PageStepStop -> pure (acc ++ payouts)
        PageStepContinue cursor -> go (succ pagesFetched) (Just cursor) (acc ++ payouts)
        PageStepStuck cursor -> throwE $ StripeApiErrorPagingStuck url cursor
        PageStepTooManyPages limit -> throwE $ StripeApiErrorTooManyPages url limit

{-# ANN getJSON ("NOCOVER" :: String) #-}
getJSON ::
  (HasCodec a) =>
  Manager ->
  StripeKey ->
  String ->
  [(ByteString, ByteString)] ->
  LoggingT IO (Either StripeApiError a)
getJSON man key url query = do
  raw <- request man key url Nothing query
  pure $ raw >>= decodeBody url

{-# ANN postJSON ("NOCOVER" :: String) #-}
postJSON ::
  (HasCodec a) =>
  Manager ->
  StripeKey ->
  String ->
  [(ByteString, ByteString)] ->
  LoggingT IO (Either StripeApiError a)
postJSON man key url body = do
  raw <- request man key url (Just body) []
  pure $ raw >>= decodeBody url

decodeBody :: (HasCodec a) => String -> LB.ByteString -> Either StripeApiError a
decodeBody url body = case eitherDecodeJSONViaCodec body of
  Left reason -> Left $ StripeApiErrorDecode url reason
  Right a -> Right a

{-# ANN getRaw ("NOCOVER" :: String) #-}
getRaw :: Manager -> StripeKey -> String -> LoggingT IO (Either StripeApiError LB.ByteString)
getRaw man key url = request man key url Nothing []

{-# ANN request ("NOCOVER" :: String) #-}
request ::
  Manager ->
  StripeKey ->
  String ->
  -- | A form body, which makes this a POST.
  Maybe [(ByteString, ByteString)] ->
  [(ByteString, ByteString)] ->
  LoggingT IO (Either StripeApiError LB.ByteString)
request man key url mBody query = do
  logDebugN $ Text.pack $ unwords [maybe "GET" (const "POST") mBody, url]
  errOrResponse <- liftIO $ try $ do
    initial <- HTTP.parseRequest url
    let authorised =
          initial
            { requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 (unStripeKey key)),
                  ("Stripe-Version", stripeVersion)
                ]
            }
    let withQuery = HTTP.setQueryString (map (fmap Just) query) authorised
    let final = maybe withQuery (\body -> HTTP.urlEncodedBody body withQuery) mBody
    HTTP.httpLbs final man
  case errOrResponse of
    Left (e :: HttpException) -> pure $ Left $ StripeApiErrorHttp url e
    Right response ->
      let status = HTTP.statusCode (responseStatus response)
          body = responseBody response
       in pure $
            if status /= 200
              then Left $ StripeApiErrorStatus url status body
              else Right body

{-# ANN requireApi ("NOCOVER" :: String) #-}
requireApi :: LoggingT IO (Either StripeApiError a) -> StripeM a
requireApi = ExceptT

-- | Pin the API version, so that Stripe moving a field is a decode failure on a run
-- the user chose to make rather than a surprise on an unchanged importer.
stripeVersion :: ByteString
stripeVersion = "2026-07-29.dahlia"

newtype ReportRunId = ReportRunId {unReportRunId :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity ReportRunId

instance HasCodec ReportRunId where
  codec = dimapCodec ReportRunId unReportRunId codec

data ReportRunStatus
  = ReportRunStatusPending
  | ReportRunStatusSucceeded
  | ReportRunStatusFailed
  deriving stock (Show, Eq, Generic)

instance Validity ReportRunStatus

instance HasCodec ReportRunStatus where
  codec =
    stringConstCodec
      ( (ReportRunStatusPending, "pending")
          :| [ (ReportRunStatusSucceeded, "succeeded"),
               (ReportRunStatusFailed, "failed")
             ]
      )

-- | One run of a report over one interval.
data ReportRun = ReportRun
  { reportRunId :: !ReportRunId,
    reportRunStatus :: !ReportRunStatus,
    -- | Where the finished CSV can be fetched, once the run has succeeded.
    reportRunResultUrl :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ReportRun

instance HasCodec ReportRun where
  codec =
    object "ReportRun" $
      ReportRun
        <$> requiredField "id" "identifier" .= reportRunId
        <*> requiredField "status" "how far along this run is" .= reportRunStatus
        <*> optionalFieldOrNullWith
          "result"
          (object "ReportRunResult" $ requiredField "url" "where to fetch the CSV")
          "the finished file"
          .= reportRunResultUrl

-- | One page of a Stripe list response.
data StripeList a = StripeList
  { stripeListData :: ![a],
    stripeListHasMore :: !Bool
  }
  deriving stock (Show, Eq, Generic)

instance (Validity a) => Validity (StripeList a)

instance (HasCodec a) => HasCodec (StripeList a) where
  codec =
    object "StripeList" $
      StripeList
        <$> requiredField "data" "this page" .= stripeListData
        <*> requiredField "has_more" "whether there is another page" .= stripeListHasMore
