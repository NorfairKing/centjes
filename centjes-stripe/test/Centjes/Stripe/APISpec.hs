{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Stripe.APISpec (spec) where

import Autodocodec.Aeson (eitherDecodeJSONViaCodec)
import Centjes.Stripe.API
import Centjes.Stripe.Gen (codecSpec)
import Centjes.Stripe.Payout
import Test.Syd

spec :: Spec
spec = do
  codecSpec @ReportRunId
  codecSpec @ReportRunStatus
  codecSpec @ReportRun
  codecSpec @(StripeList Payout)

  describe "ReportRun" $ do
    -- The url is nested inside a result object, and absent entirely until the run
    -- finishes.
    it "reads the result url out of a finished run" $
      eitherDecodeJSONViaCodec
        "{\"id\":\"frr_1\",\"status\":\"succeeded\",\"result\":{\"url\":\"https://files.stripe.com/v1/files/f_1/contents\"}}"
        `shouldBe` Right
          ReportRun
            { reportRunId = ReportRunId "frr_1",
              reportRunStatus = ReportRunStatusSucceeded,
              reportRunResultUrl = Just "https://files.stripe.com/v1/files/f_1/contents"
            }

    it "reads a pending run that has no result yet" $
      eitherDecodeJSONViaCodec "{\"id\":\"frr_1\",\"status\":\"pending\",\"result\":null}"
        `shouldBe` Right
          ReportRun
            { reportRunId = ReportRunId "frr_1",
              reportRunStatus = ReportRunStatusPending,
              reportRunResultUrl = Nothing
            }

  describe "pageStep" $ do
    it "stops when Stripe says there is no more" $
      pageStep 1 Nothing False (Just "po_1") `shouldBe` PageStepStop

    it "continues from the last object of the page" $
      pageStep 1 Nothing True (Just "po_1") `shouldBe` PageStepContinue "po_1"

    -- Stripe saying there is more while sending nothing to continue from would
    -- otherwise ask for the same page forever.
    it "stops when there is more but nothing to continue from" $
      pageStep 1 Nothing True Nothing `shouldBe` PageStepStop

    it "refuses to ask again for the cursor it already asked for" $
      pageStep 2 (Just "po_1") True (Just "po_1") `shouldBe` PageStepStuck "po_1"

    it "gives up past the page bound" $
      pageStep maximumPages (Just "po_1") True (Just "po_2")
        `shouldBe` PageStepTooManyPages maximumPages

    it "does not call a finished listing a runaway" $
      pageStep maximumPages (Just "po_1") False (Just "po_2") `shouldBe` PageStepStop

  describe "pollStep" $ do
    let run status url =
          ReportRun
            { reportRunId = ReportRunId "frr_1",
              reportRunStatus = status,
              reportRunResultUrl = url
            }

    it "fetches from the url of a finished run" $
      pollStep 1 (run ReportRunStatusSucceeded (Just "https://files.stripe.com/x"))
        `shouldBe` PollStepReady "https://files.stripe.com/x"

    it "waits on a pending run" $
      pollStep 1 (run ReportRunStatusPending Nothing) `shouldBe` PollStepWait

    it "gives up on a failed run rather than waiting for it" $
      pollStep 1 (run ReportRunStatusFailed Nothing) `shouldBe` PollStepFailed

    -- Succeeded with nowhere to fetch from would otherwise be read as a pending run and
    -- waited on until the poll bound.
    it "does not wait on a finished run that named no url" $
      pollStep 1 (run ReportRunStatusSucceeded Nothing) `shouldBe` PollStepNoUrl

    it "gives up past the poll bound" $
      pollStep maximumPolls (run ReportRunStatusPending Nothing)
        `shouldBe` PollStepTimedOut maximumPolls

    -- Being past the bound must not stop a run that has actually finished.
    it "still fetches a finished run past the poll bound" $
      pollStep maximumPolls (run ReportRunStatusSucceeded (Just "https://files.stripe.com/x"))
        `shouldBe` PollStepReady "https://files.stripe.com/x"
