{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import           IEXHaskellSDK.Model
import           IEXHaskellSDK.Core

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Time                     as TI
import qualified Data.Vector                   as V

import           Control.Monad
import           Data.Char                      ( isSpace )
import           Data.List                      ( sort )
import           Test.QuickCheck

import           ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink    = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary
  shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
  arbitrary = ByteArray <$> arbitrary
  shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
  arbitrary = Binary <$> arbitrary
  shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
  arbitrary = DateTime <$> arbitrary
  shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
  arbitrary = Date <$> arbitrary
  shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
   where
    simpleTypes :: Gen A.Value
    simpleTypes = frequency
      [ (1, return A.Null)
      , (2, liftM A.Bool (arbitrary :: Gen Bool))
      , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
      , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
      ]
    mapF (k, v) = (T.pack k, v)
    simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
    arrayTypes      = sized sizedArray
    objectTypes     = sized sizedObject
    sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
    sizedObject n =
      liftM (A.object . map mapF)
        $   replicateM n
        $   (,)
        <$> (arbitrary :: Gen String)
        <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups :: (Ord a) => [a] -> Bool
hasNoDups = go Set.empty
 where
  go _ [] = True
  go s (x : xs) | s' <- Set.insert x s, Set.size s' > Set.size s = go s' xs
                | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null then return Nothing else return generated

-- * Models

instance Arbitrary Article where
  arbitrary = sized genArticle

genArticle :: Int -> Gen Article
genArticle n =
  Article
    <$> arbitraryReducedMaybe n -- articleDatetime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- articleHeadline :: Maybe Text
    <*> arbitraryReducedMaybe n -- articleSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- articleUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- articleSummary :: Maybe Text
    <*> arbitraryReducedMaybe n -- articleRelated :: Maybe Text
    <*> arbitraryReducedMaybe n -- articleImage :: Maybe Text

instance Arbitrary Auction where
  arbitrary = sized genAuction

genAuction :: Int -> Gen Auction
genAuction n =
  Auction
    <$> arbitraryReducedMaybe n -- auctionAuctionType :: Maybe E'AuctionType
    <*> arbitraryReducedMaybe n -- auctionPairedShares :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionImbalanceShares :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionReferencePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionIndicativePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionAuctionBookPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionCollarReferencePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionLowerCollarPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionUpperCollarPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionExtensionNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- auctionStartTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- auctionLastUpdate :: Maybe Double

instance Arbitrary Chart1d where
  arbitrary = sized genChart1d

genChart1d :: Int -> Gen Chart1d
genChart1d n =
  Chart1d
    <$> arbitraryReducedMaybe n -- chart1dMinute :: Maybe Text
    <*> arbitraryReducedMaybe n -- chart1dMarketAverage :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketNotional :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketNumberOfTrades :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketOpen :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketClose :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketLow :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dMarketChangeOverTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dAverage :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dNotional :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dNumberOfTrades :: Maybe Double
    <*> arbitraryReducedMaybe n -- chart1dSimplifyFactor :: Maybe [Double]

instance Arbitrary ChartAll where
  arbitrary = sized genChartAll

genChartAll :: Int -> Gen ChartAll
genChartAll n =
  ChartAll
    <$> arbitraryReducedMaybe n -- chartAllHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartAllLow :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartAllVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartAllLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- chartAllChangeOverTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartAllDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- chartAllOpen :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartAllClose :: Maybe Double

instance Arbitrary ChartNot1d where
  arbitrary = sized genChartNot1d

genChartNot1d :: Int -> Gen ChartNot1d
genChartNot1d n =
  ChartNot1d
    <$> arbitraryReducedMaybe n -- chartNot1dUnadjustedVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartNot1dChange :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartNot1dChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- chartNot1dVwap :: Maybe Double

instance Arbitrary CorporateAction where
  arbitrary = sized genCorporateAction

genCorporateAction :: Int -> Gen CorporateAction
genCorporateAction n =
  CorporateAction
    <$> arbitraryReducedMaybe n -- corporateActionRecordId :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionDailyListTimestamp :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionEffectiveDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionIssueEvent :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentSymbolinInetSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentSymbolinCqsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentSymbolinCmsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewSymbolinInetSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewSymbolinCqsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewSymbolinCmsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentSecurityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewSecurityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentListingCenter :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewListingCenter :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionDelistingReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentRoundLotSize :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewRoundLotSize :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentLuldTierIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewLuldTierIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionExpirationDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionSeparationDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionSettlementDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionMaturityDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionRedemptionDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionCurrentFinancialStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNewFinancialStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionWhenIssuedFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionWhenDistributedFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionIpoFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionHistoryHold :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionNotesforEachEntry :: Maybe Text
    <*> arbitraryReducedMaybe n -- corporateActionRecordUpdateTime :: Maybe Text

instance Arbitrary DividendCoreData where
  arbitrary = sized genDividendCoreData

genDividendCoreData :: Int -> Gen DividendCoreData
genDividendCoreData n =
  DividendCoreData
    <$> arbitraryReducedMaybe n -- dividendCoreDataRecordId :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataDailyListTimestamp :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataSymbolinInetSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataSymbolinCqsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataSymbolyinCmsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataSecurityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataAmountDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataPaymentFrequency :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataExDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataDividendTypeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataStockAdjustmentFactor :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataStockAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataCashAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataPostSplitShares :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataPreSplitShares :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataQualifiedDividend :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataExercisePriceAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataElectionorExpirationDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataNetAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataBasisNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataNotesforEachEntry :: Maybe Text
    <*> arbitraryReducedMaybe n -- dividendCoreDataRecordUpdateTime :: Maybe Text

instance Arbitrary Earnings where
  arbitrary = sized genEarnings

genEarnings :: Int -> Gen Earnings
genEarnings n =
  Earnings
    <$> arbitraryReducedMaybe n -- earningsActualEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsConsensusEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsEstimatedEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsAnnounceTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- earningsNumberOfEstimates :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsEpsSurpriseDollar :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsEpsReportDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- earningsFiscalPeriod :: Maybe Text
    <*> arbitraryReducedMaybe n -- earningsFiscalEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- earningsYearAgo :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsYearAgoChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsEstimatedChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- earningsSymbolId :: Maybe Double

instance Arbitrary Financials where
  arbitrary = sized genFinancials

genFinancials :: Int -> Gen Financials
genFinancials n =
  Financials
    <$> arbitraryReducedMaybe n -- financialsReportDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- financialsGrossProfit :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsCostOfRevenue :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsOperatingRevenue :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsTotalRevenue :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsOperatingIncome :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsNetIncome :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsResearchAndDevelopment :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsOperatingExpense :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsCurrentAssets :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsTotalAssets :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsTotalLiabilities :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsCurrentCash :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsCurrentDebt :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsTotalCash :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsTotalDebt :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsShareholderEquity :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsCashChange :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsCashFlow :: Maybe Double
    <*> arbitraryReducedMaybe n -- financialsOperatingGainsLosses :: Maybe Text

instance Arbitrary DeepBookGetResponse where
  arbitrary = sized genDeepBookGetResponse

genDeepBookGetResponse :: Int -> Gen DeepBookGetResponse
genDeepBookGetResponse n =
  DeepBookGetResponse
    <$> arbitraryReducedMaybe n -- deepBookGetResponseBids :: Maybe [Order]
    <*> arbitraryReducedMaybe n -- deepBookGetResponseAsks :: Maybe [Order]

instance Arbitrary DeepGetResponse where
  arbitrary = sized genDeepGetResponse

genDeepGetResponse :: Int -> Gen DeepGetResponse
genDeepGetResponse n =
  DeepGetResponse
    <$> arbitraryReducedMaybe n -- DeepGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseMarketPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseLastSalePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseLastSaleSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseLastSaleTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseLastUpdated :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseBids :: Maybe [Order]
    <*> arbitraryReducedMaybe n -- DeepGetResponseAsks :: Maybe [Order]
    <*> arbitraryReducedMaybe n -- DeepGetResponseSystemEvent :: Maybe [SystemEvent]
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradingStatus :: Maybe DeepGetResponseTradingStatus
    <*> arbitraryReducedMaybe n -- DeepGetResponseOpHaltStatus :: Maybe DeepGetResponseOpHaltStatus
    <*> arbitraryReducedMaybe n -- DeepGetResponseSsrStatus :: Maybe DeepGetResponseSsrStatus
    <*> arbitraryReducedMaybe n -- DeepGetResponseSecurityEvent :: Maybe DeepGetResponseSecurityEvent
    <*> arbitraryReducedMaybe n -- DeepGetResponseTrades :: Maybe [Trade]
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaks :: Maybe [DeepGetResponseTradeBreaks]
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuction :: Maybe DeepGetResponseAuction

instance Arbitrary StatsHistoricalDailyGetResponse where
  arbitrary = sized genStatsHistoricalDailyGetResponse

genStatsHistoricalDailyGetResponse :: Int -> Gen StatsHistoricalDailyGetResponse
genStatsHistoricalDailyGetResponse n =
  StatsHistoricalDailyGetResponse
    <$> arbitraryReducedMaybe n -- statsHistoricalDailyGetResponseDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- statsHistoricalDailyGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsHistoricalDailyGetResponseRoutedVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsHistoricalDailyGetResponseMarketShare :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsHistoricalDailyGetResponseIsHalfday :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsHistoricalDailyGetResponseLitVolume :: Maybe Double

instance Arbitrary StatsIntradayGetResponse where
  arbitrary = sized genStatsIntradayGetResponse

genStatsIntradayGetResponse :: Int -> Gen StatsIntradayGetResponse
genStatsIntradayGetResponse n =
  StatsIntradayGetResponse
    <$> arbitraryReducedMaybe n -- statsIntradayGetResponseVolume :: Maybe TimestampedValue
    <*> arbitraryReducedMaybe n -- statsIntradayGetResponseSymbolsTraded :: Maybe TimestampedValue
    <*> arbitraryReducedMaybe n -- statsIntradayGetResponseRoutedVolume :: Maybe TimestampedValue
    <*> arbitraryReducedMaybe n -- statsIntradayGetResponseNotional :: Maybe TimestampedValue

instance Arbitrary StatsRecentGetResponse where
  arbitrary = sized genStatsRecentGetResponse

genStatsRecentGetResponse :: Int -> Gen StatsRecentGetResponse
genStatsRecentGetResponse n =
  StatsRecentGetResponse
    <$> arbitraryReducedMaybe n -- statsRecentGetResponseDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- statsRecentGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsRecentGetResponseRoutedVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsRecentGetResponseMarketShare :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsRecentGetResponseIsHalfday :: Maybe Bool
    <*> arbitraryReducedMaybe n -- statsRecentGetResponseLitVolume :: Maybe Double

instance Arbitrary StatsRecordsGetResponse where
  arbitrary = sized genStatsRecordsGetResponse

genStatsRecordsGetResponse :: Int -> Gen StatsRecordsGetResponse
genStatsRecordsGetResponse n =
  StatsRecordsGetResponse
    <$> arbitraryReducedMaybe n -- statsRecordsGetResponseVolume :: Maybe RecordedValue
    <*> arbitraryReducedMaybe n -- statsRecordsGetResponseSymbolsTraded :: Maybe RecordedValue
    <*> arbitraryReducedMaybe n -- statsRecordsGetResponseRoutedVolume :: Maybe RecordedValue
    <*> arbitraryReducedMaybe n -- statsRecordsGetResponseNotional :: Maybe RecordedValue

instance Arbitrary MarketGetResponse where
  arbitrary = sized genMarketGetResponse

genMarketGetResponse :: Int -> Gen MarketGetResponse
genMarketGetResponse n =
  MarketGetResponse
    <$> arbitraryReducedMaybe n -- marketGetResponseMic :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketGetResponseTapeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketGetResponseVenueName :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- marketGetResponseTapeA :: Maybe Double
    <*> arbitraryReducedMaybe n -- marketGetResponseTapeB :: Maybe Double
    <*> arbitraryReducedMaybe n -- marketGetResponseTapeC :: Maybe Double
    <*> arbitraryReducedMaybe n -- marketGetResponseMarketPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- marketGetResponseLastUpdated :: Maybe Double

instance Arbitrary RefDataSymbolsGetResponse where
  arbitrary = sized genRefDataSymbolsGetResponse

genRefDataSymbolsGetResponse :: Int -> Gen RefDataSymbolsGetResponse
genRefDataSymbolsGetResponse n =
  RefDataSymbolsGetResponse
    <$> arbitraryReducedMaybe n -- refDataSymbolsGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- refDataSymbolsGetResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- refDataSymbolsGetResponseDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- refDataSymbolsGetResponseIsEnabled :: Maybe Text
    <*> arbitraryReducedMaybe n -- refDataSymbolsGetResponseType :: Maybe E'Type
    <*> arbitraryReducedMaybe n -- refDataSymbolsGetResponseIexId :: Maybe Double

instance Arbitrary StockSymbolBookGetResponse where
  arbitrary = sized genStockSymbolBookGetResponse

genStockSymbolBookGetResponse :: Int -> Gen StockSymbolBookGetResponse
genStockSymbolBookGetResponse n =
  StockSymbolBookGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolBookGetResponseQuote :: Maybe Quote
    <*> arbitraryReducedMaybe n -- stockSymbolBookGetResponseBids :: Maybe [Order]
    <*> arbitraryReducedMaybe n -- stockSymbolBookGetResponseAsks :: Maybe [Order]
    <*> arbitraryReducedMaybe n -- stockSymbolBookGetResponseTrades :: Maybe [Trade]
    <*> arbitraryReducedMaybe n -- stockSymbolBookGetResponseSystemEvent :: Maybe SystemEvent

instance Arbitrary StockSymbolChartDynamicGetResponse where
  arbitrary = sized genStockSymbolChartDynamicGetResponse

genStockSymbolChartDynamicGetResponse :: Int -> Gen StockSymbolChartDynamicGetResponse
genStockSymbolChartDynamicGetResponse n =
  StockSymbolChartDynamicGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolChartDynamicGetResponseRange :: Maybe E'Range
    <*> arbitraryReducedMaybe n -- stockSymbolChartDynamicGetResponseData :: Maybe [ChartAll]

instance Arbitrary StockSymbolCompanyGetResponse where
  arbitrary = sized genStockSymbolCompanyGetResponse

genStockSymbolCompanyGetResponse :: Int -> Gen StockSymbolCompanyGetResponse
genStockSymbolCompanyGetResponse n =
  StockSymbolCompanyGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseExchange :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseIndustry :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseCeo :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseIssueType :: Maybe E'IssueType
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseSector :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolCompanyGetResponseTags :: Maybe [Sector]

instance Arbitrary StockSymbolDelayedQuoteGetResponse where
  arbitrary = sized genStockSymbolDelayedQuoteGetResponse

genStockSymbolDelayedQuoteGetResponse :: Int -> Gen StockSymbolDelayedQuoteGetResponse
genStockSymbolDelayedQuoteGetResponse n =
  StockSymbolDelayedQuoteGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolDelayedQuoteGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolDelayedQuoteGetResponseDelayedPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolDelayedQuoteGetResponseDelayedSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolDelayedQuoteGetResponseDelayedPriceTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolDelayedQuoteGetResponseProcessedTime :: Maybe Double

instance Arbitrary DeepGetResponseAuction where
  arbitrary = sized genDeepGetResponseAuction

genDeepGetResponseAuction :: Int -> Gen DeepGetResponseAuction
genDeepGetResponseAuction n =
  DeepGetResponseAuction
    <$> arbitraryReducedMaybe n -- DeepGetResponseAuctionAuctionType :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionPairedShares :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionImbalanceShares :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionReferencePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionIndicativePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionAuctionBookProce :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionCollarReferencePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionLowerCollarPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionUpperCollarPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionExtensionNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionStartTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseAuctionLastUpdate :: Maybe Double

instance Arbitrary DeepGetResponseOpHaltStatus where
  arbitrary = sized genDeepGetResponseOpHaltStatus

genDeepGetResponseOpHaltStatus :: Int -> Gen DeepGetResponseOpHaltStatus
genDeepGetResponseOpHaltStatus n =
  DeepGetResponseOpHaltStatus
    <$> arbitraryReducedMaybe n -- DeepGetResponseOpHaltStatusIsHalted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseOpHaltStatusTimestamp :: Maybe Double

instance Arbitrary DeepGetResponseSecurityEvent where
  arbitrary = sized genDeepGetResponseSecurityEvent

genDeepGetResponseSecurityEvent :: Int -> Gen DeepGetResponseSecurityEvent
genDeepGetResponseSecurityEvent n =
  DeepGetResponseSecurityEvent
    <$> arbitraryReducedMaybe n -- DeepGetResponseSecurityEventSecurityEvent :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseSecurityEventTimestamp :: Maybe Double

instance Arbitrary DeepGetResponseSsrStatus where
  arbitrary = sized genDeepGetResponseSsrStatus

genDeepGetResponseSsrStatus :: Int -> Gen DeepGetResponseSsrStatus
genDeepGetResponseSsrStatus n =
  DeepGetResponseSsrStatus
    <$> arbitraryReducedMaybe n -- DeepGetResponseSsrStatusIsSsr :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseSsrStatusDetail :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseSsrStatusTimestamp :: Maybe Double

instance Arbitrary DeepGetResponseTradeBreaks where
  arbitrary = sized genDeepGetResponseTradeBreaks

genDeepGetResponseTradeBreaks :: Int -> Gen DeepGetResponseTradeBreaks
genDeepGetResponseTradeBreaks n =
  DeepGetResponseTradeBreaks
    <$> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksTradeId :: Maybe Double
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksIsIso :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksIsOddLot :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksIsOutsideRegularHours :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksIsSinglePriceCross :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksIsTradeThroughExempt :: Maybe Bool
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradeBreaksTimestamp :: Maybe Double

instance Arbitrary DeepGetResponseTradingStatus where
  arbitrary = sized genDeepGetResponseTradingStatus

genDeepGetResponseTradingStatus :: Int -> Gen DeepGetResponseTradingStatus
genDeepGetResponseTradingStatus n =
  DeepGetResponseTradingStatus
    <$> arbitraryReducedMaybe n -- DeepGetResponseTradingStatusStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradingStatusReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- DeepGetResponseTradingStatusTimestamp :: Maybe Double

instance Arbitrary HistGetResponse where
  arbitrary = sized genHistGetResponse

genHistGetResponse :: Int -> Gen HistGetResponse
genHistGetResponse n =
  HistGetResponse
    <$> arbitraryReducedMaybe n -- histGetResponseLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- histGetResponseDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- histGetResponseFeed :: Maybe Text
    <*> arbitraryReducedMaybe n -- histGetResponseVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- histGetResponseProtocol :: Maybe Text
    <*> arbitraryReducedMaybe n -- histGetResponseSize :: Maybe Text

instance Arbitrary StockSymbolDividendsRangeGetResponse where
  arbitrary = sized genStockSymbolDividendsRangeGetResponse

genStockSymbolDividendsRangeGetResponse :: Int -> Gen StockSymbolDividendsRangeGetResponse
genStockSymbolDividendsRangeGetResponse n =
  StockSymbolDividendsRangeGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseExDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponsePaymentDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseRecordDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseDeclaredDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseFlag :: Maybe E'Flag
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseType :: Maybe E'Type2
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseQualified :: Maybe E'Qualified
    <*> arbitraryReducedMaybe n -- stockSymbolDividendsRangeGetResponseIndicated :: Maybe Double

instance Arbitrary StockSymbolEarningsGetResponse where
  arbitrary = sized genStockSymbolEarningsGetResponse

genStockSymbolEarningsGetResponse :: Int -> Gen StockSymbolEarningsGetResponse
genStockSymbolEarningsGetResponse n =
  StockSymbolEarningsGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolEarningsGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolEarningsGetResponseEarnings :: Maybe [Earnings]

instance Arbitrary StockSymbolEffectiveSpreadGetResponse where
  arbitrary = sized genStockSymbolEffectiveSpreadGetResponse

genStockSymbolEffectiveSpreadGetResponse :: Int -> Gen StockSymbolEffectiveSpreadGetResponse
genStockSymbolEffectiveSpreadGetResponse n =
  StockSymbolEffectiveSpreadGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolEffectiveSpreadGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolEffectiveSpreadGetResponseVenue :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolEffectiveSpreadGetResponseVenueName :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolEffectiveSpreadGetResponseEffectiveSpread :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolEffectiveSpreadGetResponseEffectiveQuoted :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolEffectiveSpreadGetResponsePriceImprovement :: Maybe Double

instance Arbitrary StockSymbolFinancialsGetResponse where
  arbitrary = sized genStockSymbolFinancialsGetResponse

genStockSymbolFinancialsGetResponse :: Int -> Gen StockSymbolFinancialsGetResponse
genStockSymbolFinancialsGetResponse n =
  StockSymbolFinancialsGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolFinancialsGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolFinancialsGetResponseFinancials :: Maybe [Financials]

instance Arbitrary StockMarketThresholdSecuritiesGetResponse where
  arbitrary = sized genStockMarketThresholdSecuritiesGetResponse

genStockMarketThresholdSecuritiesGetResponse :: Int -> Gen StockMarketThresholdSecuritiesGetResponse
genStockMarketThresholdSecuritiesGetResponse n =
  StockMarketThresholdSecuritiesGetResponse
    <$> arbitraryReducedMaybe n -- stockMarketThresholdSecuritiesGetResponseTradeDate :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockMarketThresholdSecuritiesGetResponseSymbolinInetSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockMarketThresholdSecuritiesGetResponseSymbolinCqsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockMarketThresholdSecuritiesGetResponseSymbolinCmsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockMarketThresholdSecuritiesGetResponseSecurityName :: Maybe Text

instance Arbitrary StockMarketUpcomingIposGetResponse where
  arbitrary = sized genStockMarketUpcomingIposGetResponse

genStockMarketUpcomingIposGetResponse :: Int -> Gen StockMarketUpcomingIposGetResponse
genStockMarketUpcomingIposGetResponse n =
  StockMarketUpcomingIposGetResponse
    <$> arbitraryReducedMaybe n -- stockMarketUpcomingIposGetResponseRawData :: Maybe [IpoRawData]
    <*> arbitraryReducedMaybe n -- stockMarketUpcomingIposGetResponseViewData :: Maybe [IpoViewData]

instance Arbitrary StockSymbolStatsGetResponse where
  arbitrary = sized genStockSymbolStatsGetResponse

genStockSymbolStatsGetResponse :: Int -> Gen StockSymbolStatsGetResponse
genStockSymbolStatsGetResponse n =
  StockSymbolStatsGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseMarketcap :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseBeta :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Week52high :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Week52low :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Week52change :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseShortInterest :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseShortDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseDividendRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseDividendYield :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseExDividendDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseLatestEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseLatestEpsDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseSharesOutstanding :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseFloat :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseReturnOnEquity :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseConsensusEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseNumberOfEstimates :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseEbitda :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseRevenue :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseGrossProfit :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseCash :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseDebt :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseTtmEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseRevenuePerShare :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseRevenuePerEmployee :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponsePeRatioHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponsePeRatioLow :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseEpsSurpriseDollar :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseEpsSurprisePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseReturnOnAssets :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseReturnOnCapital :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseProfitMargin :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponsePriceToSales :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponsePriceToBook :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Day200MovingAvg :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Day50MovingAvg :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseInstitutionPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseInsiderPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseShortRatio :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Year5ChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Year2ChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Year1ChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolStatsGetResponseYtdChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Month6ChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Month3ChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Month1ChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse20026Day5ChangePercent :: Maybe Double

instance Arbitrary StockSymbolLargestTradesGetResponse where
  arbitrary = sized genStockSymbolLargestTradesGetResponse

genStockSymbolLargestTradesGetResponse :: Int -> Gen StockSymbolLargestTradesGetResponse
genStockSymbolLargestTradesGetResponse n =
  StockSymbolLargestTradesGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolLargestTradesGetResponsePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolLargestTradesGetResponseSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolLargestTradesGetResponseTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolLargestTradesGetResponseTimeLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolLargestTradesGetResponseVenue :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolLargestTradesGetResponseVenueName :: Maybe Text

instance Arbitrary StockSymbolLogoGetResponse where
  arbitrary = sized genStockSymbolLogoGetResponse

genStockSymbolLogoGetResponse :: Int -> Gen StockSymbolLogoGetResponse
genStockSymbolLogoGetResponse n = StockSymbolLogoGetResponse <$> arbitraryReducedMaybe n -- stockSymbolLogoGetResponseUrl :: Maybe Text

instance Arbitrary StockSymbolRelevantGetResponse where
  arbitrary = sized genStockSymbolRelevantGetResponse

genStockSymbolRelevantGetResponse :: Int -> Gen StockSymbolRelevantGetResponse
genStockSymbolRelevantGetResponse n =
  StockSymbolRelevantGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolRelevantGetResponsePeers :: Maybe Bool
    <*> arbitraryReducedMaybe n -- stockSymbolRelevantGetResponseSymbols :: Maybe [Text]

instance Arbitrary TopsLastGetResponse where
  arbitrary = sized genTopsLastGetResponse

genTopsLastGetResponse :: Int -> Gen TopsLastGetResponse
genTopsLastGetResponse n =
  TopsLastGetResponse
    <$> arbitraryReducedMaybe n -- topsLastGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- topsLastGetResponsePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsLastGetResponseSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsLastGetResponseTime :: Maybe Double

instance Arbitrary StockMarketSectorPerformanceGetResponse where
  arbitrary = sized genStockMarketSectorPerformanceGetResponse

genStockMarketSectorPerformanceGetResponse :: Int -> Gen StockMarketSectorPerformanceGetResponse
genStockMarketSectorPerformanceGetResponse n =
  StockMarketSectorPerformanceGetResponse
    <$> arbitraryReducedMaybe n -- stockMarketSectorPerformanceGetResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockMarketSectorPerformanceGetResponseName :: Maybe Sector
    <*> arbitraryReducedMaybe n -- stockMarketSectorPerformanceGetResponsePerformance :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockMarketSectorPerformanceGetResponseLastUpdated :: Maybe Double

instance Arbitrary StocksSymbolSplitsRangeGetResponse where
  arbitrary = sized genStocksSymbolSplitsRangeGetResponse

genStocksSymbolSplitsRangeGetResponse :: Int -> Gen StocksSymbolSplitsRangeGetResponse
genStocksSymbolSplitsRangeGetResponse n =
  StocksSymbolSplitsRangeGetResponse
    <$> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponseExDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponseDeclaredDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponseRecordDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponsePaymentDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponseRatio :: Maybe Double
    <*> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponseToFactor :: Maybe Text
    <*> arbitraryReducedMaybe n -- stocksSymbolSplitsRangeGetResponseForFactor :: Maybe Text

instance Arbitrary StockMarketTodayEarningsGetResponse where
  arbitrary = sized genStockMarketTodayEarningsGetResponse

genStockMarketTodayEarningsGetResponse :: Int -> Gen StockMarketTodayEarningsGetResponse
genStockMarketTodayEarningsGetResponse n =
  StockMarketTodayEarningsGetResponse
    <$> arbitraryReducedMaybe n -- stockMarketTodayEarningsGetResponseBto :: Maybe [TodayEarnings]
    <*> arbitraryReducedMaybe n -- stockMarketTodayEarningsGetResponseAmc :: Maybe [TodayEarnings]

instance Arbitrary StockSymbolVolumeByVenueGetResponse where
  arbitrary = sized genStockSymbolVolumeByVenueGetResponse

genStockSymbolVolumeByVenueGetResponse :: Int -> Gen StockSymbolVolumeByVenueGetResponse
genStockSymbolVolumeByVenueGetResponse n =
  StockSymbolVolumeByVenueGetResponse
    <$> arbitraryReducedMaybe n -- stockSymbolVolumeByVenueGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolVolumeByVenueGetResponseVenue :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolVolumeByVenueGetResponseVenueName :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockSymbolVolumeByVenueGetResponseDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- stockSymbolVolumeByVenueGetResponseMarketPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- stockSymbolVolumeByVenueGetResponseAvgMarketPercent :: Maybe Double

instance Arbitrary DeepOfficialPriceGetResponse where
  arbitrary = sized genDeepOfficialPriceGetResponse

genDeepOfficialPriceGetResponse :: Int -> Gen DeepOfficialPriceGetResponse
genDeepOfficialPriceGetResponse n =
  DeepOfficialPriceGetResponse
    <$> arbitraryReducedMaybe n -- deepOfficialPriceGetResponsePriceType :: Maybe E'PriceType
    <*> arbitraryReducedMaybe n -- deepOfficialPriceGetResponsePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- deepOfficialPriceGetResponseTimestamp :: Maybe Double

instance Arbitrary DeepOpHaltStatusGetResponse where
  arbitrary = sized genDeepOpHaltStatusGetResponse

genDeepOpHaltStatusGetResponse :: Int -> Gen DeepOpHaltStatusGetResponse
genDeepOpHaltStatusGetResponse n =
  DeepOpHaltStatusGetResponse
    <$> arbitraryReducedMaybe n -- deepOpHaltStatusGetResponseIsHalted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- deepOpHaltStatusGetResponseTimeStamp :: Maybe Double

instance Arbitrary DeepSecurityEventGetResponse where
  arbitrary = sized genDeepSecurityEventGetResponse

genDeepSecurityEventGetResponse :: Int -> Gen DeepSecurityEventGetResponse
genDeepSecurityEventGetResponse n =
  DeepSecurityEventGetResponse
    <$> arbitraryReducedMaybe n -- deepSecurityEventGetResponseSecurityEvent :: Maybe E'SecurityEvent
    <*> arbitraryReducedMaybe n -- deepSecurityEventGetResponseTimestamp :: Maybe Double

instance Arbitrary DeepSystemEventGetResponse where
  arbitrary = sized genDeepSystemEventGetResponse

genDeepSystemEventGetResponse :: Int -> Gen DeepSystemEventGetResponse
genDeepSystemEventGetResponse n =
  DeepSystemEventGetResponse
    <$> arbitraryReducedMaybe n -- deepSystemEventGetResponseSystemEvent :: Maybe SystemEvent
    <*> arbitraryReducedMaybe n -- deepSystemEventGetResponseTimestamp :: Maybe Double

instance Arbitrary TopsGetResponse where
  arbitrary = sized genTopsGetResponse

genTopsGetResponse :: Int -> Gen TopsGetResponse
genTopsGetResponse n =
  TopsGetResponse
    <$> arbitraryReducedMaybe n -- topsGetResponseSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- topsGetResponseMarketPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseBidSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseBidPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseAskSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseAskPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseLastSalePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseLastSaleSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseLastSaleTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseLastUpdated :: Maybe Double
    <*> arbitraryReducedMaybe n -- topsGetResponseSector :: Maybe Sector
    <*> arbitraryReducedMaybe n -- topsGetResponseSecurityType :: Maybe Text

instance Arbitrary DeepTradingStatusGetResponse where
  arbitrary = sized genDeepTradingStatusGetResponse

genDeepTradingStatusGetResponse :: Int -> Gen DeepTradingStatusGetResponse
genDeepTradingStatusGetResponse n =
  DeepTradingStatusGetResponse
    <$> arbitraryReducedMaybe n -- deepTradingStatusGetResponseStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- deepTradingStatusGetResponseReason :: Maybe E'Reason
    <*> arbitraryReducedMaybe n -- deepTradingStatusGetResponseTimestamp :: Maybe Double

instance Arbitrary IpoRawData where
  arbitrary = sized genIpoRawData

genIpoRawData :: Int -> Gen IpoRawData
genIpoRawData n =
  IpoRawData
    <$> arbitraryReducedMaybe n -- ipoRawDataSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataExpectedDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataLeadUnderwriters :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- ipoRawDataUnderwriters :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- ipoRawDataCompanyCounsel :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- ipoRawDataUnderWriterCounsel :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- ipoRawDataAuditor :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataMarket :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataCik :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataState :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataZip :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataCeo :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataEmployees :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataSharesOffered :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataPriceLow :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataPriceHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataOfferAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataTotalExpenses :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataSharesOverAlloted :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataShareholderShares :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataSharesOutstanding :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataLockupPeriodExpiration :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataQuietPeriodExpiration :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataRevenue :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataNetIncome :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataTotalAssets :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataTotalLiabilities :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataStockholderEquity :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataCompanyDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataBusinessDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataUseOfProceeds :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataCompetition :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoRawDataAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- ipoRawDataPercentOffered :: Maybe Double

instance Arbitrary IpoViewData where
  arbitrary = sized genIpoViewData

genIpoViewData :: Int -> Gen IpoViewData
genIpoViewData n =
  IpoViewData
    <$> arbitraryReducedMaybe n -- ipoViewDataCompany :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataShares :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataFloat :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataPercent :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataMarket :: Maybe Text
    <*> arbitraryReducedMaybe n -- ipoViewDataExpected :: Maybe Text

instance Arbitrary ListedSymbolData where
  arbitrary = sized genListedSymbolData

genListedSymbolData :: Int -> Gen ListedSymbolData
genListedSymbolData n =
  ListedSymbolData
    <$> arbitraryReducedMaybe n -- listedSymbolDataRecordId :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataDailyListTimestamp :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataSymbolinInetSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataSymbolinCqsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataSymbolinCmsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataSecurityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataTestIssue :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataIssueDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataIssueType :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataIssueSubType :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataSicCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataTransferAgent :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataFinancialStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataRoundLotSize :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataPreviousOfficialClosingPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataAdjustedPreviousOfficialClosingPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataWhenIssuedFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataWhenDistributedFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataIpoFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataFirstDateListed :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataLuldTierIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataCountryofIncorporation :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataLeveragedEtpFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataLeveragedEtpRatio :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataInverseEtpFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- listedSymbolDataRecordUpdateTime :: Maybe Text

instance Arbitrary OhlcData where
  arbitrary = sized genOhlcData

genOhlcData :: Int -> Gen OhlcData
genOhlcData n =
  OhlcData
    <$> arbitraryReducedMaybe n -- ohlcDataOpen :: Maybe OhlcDataOpen
    <*> arbitraryReducedMaybe n -- ohlcDataClose :: Maybe OhlcDataClose
    <*> arbitraryReducedMaybe n -- ohlcDataHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- ohlcDataLow :: Maybe Double

instance Arbitrary OhlcDataClose where
  arbitrary = sized genOhlcDataClose

genOhlcDataClose :: Int -> Gen OhlcDataClose
genOhlcDataClose n =
  OhlcDataClose
    <$> arbitraryReducedMaybe n -- ohlcDataClosePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- ohlcDataCloseTime :: Maybe Double

instance Arbitrary OhlcDataOpen where
  arbitrary = sized genOhlcDataOpen

genOhlcDataOpen :: Int -> Gen OhlcDataOpen
genOhlcDataOpen n =
  OhlcDataOpen
    <$> arbitraryReducedMaybe n -- ohlcDataOpenPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- ohlcDataOpenTime :: Maybe Double

instance Arbitrary Order where
  arbitrary = sized genOrder

genOrder :: Int -> Gen Order
genOrder n =
  Order
    <$> arbitraryReducedMaybe n -- orderPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- orderSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- orderTimestamp :: Maybe Double

instance Arbitrary PriceData where
  arbitrary = sized genPriceData

genPriceData :: Int -> Gen PriceData
genPriceData n =
  PriceData
    <$> arbitraryReducedMaybe n -- priceDataSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- priceDataDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- priceDataOpen :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataLow :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataClose :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataUnadjustedVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataChange :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- priceDataVwap :: Maybe Double

instance Arbitrary Quote where
  arbitrary = sized genQuote

genQuote :: Int -> Gen Quote
genQuote n =
  Quote
    <$> arbitraryReducedMaybe n -- quoteSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- quoteCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotePrimaryExchange :: Maybe Text
    <*> arbitraryReducedMaybe n -- quoteSector :: Maybe Text
    <*> arbitraryReducedMaybe n -- quoteCalculationPrice :: Maybe E'CalculationPrice
    <*> arbitraryReducedMaybe n -- quoteOpen :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteOpenTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteClose :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteCloseTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteHigh :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteLow :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteLatestPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteLatestSource :: Maybe E'LatestSource
    <*> arbitraryReducedMaybe n -- quoteLatestTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- quoteLatestUpdate :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteLatestVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexRealtimePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexRealtimeSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexLastUpdated :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteDelayedPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteDelayedPriceTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteExtendedPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteExtendedChange :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteExtendedChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteExtendedPriceTime :: Maybe Double
    <*> arbitraryReducedMaybe n -- quotePreviousClose :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteChange :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexMarketPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteAvgTotalVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexBidPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexBidSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexAskPrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteIexAskSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteMarketCap :: Maybe Double
    <*> arbitraryReducedMaybe n -- quotePeRatio :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteWeek52High :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteWeek52Low :: Maybe Double
    <*> arbitraryReducedMaybe n -- quoteYtdChange :: Maybe Double

instance Arbitrary RecordedValue where
  arbitrary = sized genRecordedValue

genRecordedValue :: Int -> Gen RecordedValue
genRecordedValue n =
  RecordedValue
    <$> arbitraryReducedMaybe n -- recordedValueRecordValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- recordedValueRecordDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- recordedValuePreviousDayValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- recordedValueAvg30Value :: Maybe Double

instance Arbitrary ShortInterest where
  arbitrary = sized genShortInterest

genShortInterest :: Int -> Gen ShortInterest
genShortInterest n =
  ShortInterest
    <$> arbitraryReducedMaybe n -- shortInterestSettlementDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestSecurityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestCurrentShortIntereste :: Maybe Double
    <*> arbitraryReducedMaybe n -- shortInterestPreviousShortInterest :: Maybe Double
    <*> arbitraryReducedMaybe n -- shortInterestPercentChange :: Maybe Double
    <*> arbitraryReducedMaybe n -- shortInterestAverageDailyVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- shortInterestDaystoCover :: Maybe Double
    <*> arbitraryReducedMaybe n -- shortInterestStockAdjustmentFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestRevisionFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestSymbolinInetSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestSymbolinCqsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestSymbolinCmsSymbology :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestNewIssueFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- shortInterestCompanyName :: Maybe Text

instance Arbitrary Stats where
  arbitrary = sized genStats

genStats :: Int -> Gen Stats
genStats n =
  Stats
    <$> arbitraryReducedMaybe n -- statsAverageDailyVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsAverageDailyRoutedVolume :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsAverageMarketShare :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsAverageOrderSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsAverageFillSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin100Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin101Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin200Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin300Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin400Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin500Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin1000Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin5000Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin10000Percent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin10000Trades :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin20000Trades :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBin50000Trades :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsUniqueSymbolsTraded :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsBlockPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsSelfCrossPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsEtfPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsLargeCapPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsMidCapPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsSmallCapPercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueArcxFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueBatsFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueBatyFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueEdgaFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueEdgxFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueOverallFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXaseFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXbosFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXchiFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXcisFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXngsFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXnysFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXphlFirstWaveWeight :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueArcxFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueBatsFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueBatyFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueEdgaFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueEdgxFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueOverallFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXaseFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXbosFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXchiFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXcisFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXngsFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXnysFirstWaveRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- statsVenueXphlFirstWaveRate :: Maybe Double

instance Arbitrary SystemEvent where
  arbitrary = sized genSystemEvent

genSystemEvent :: Int -> Gen SystemEvent
genSystemEvent n =
  SystemEvent
    <$> arbitraryReducedMaybe n -- systemEventSystemEvent :: Maybe E'SystemEvent
    <*> arbitraryReducedMaybe n -- systemEventTimestamp :: Maybe Double

instance Arbitrary TimestampedValue where
  arbitrary = sized genTimestampedValue

genTimestampedValue :: Int -> Gen TimestampedValue
genTimestampedValue n =
  TimestampedValue
    <$> arbitraryReducedMaybe n -- timestampedValueValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- timestampedValueLastUpdated :: Maybe Double

instance Arbitrary TodayEarnings where
  arbitrary = sized genTodayEarnings

genTodayEarnings :: Int -> Gen TodayEarnings
genTodayEarnings n =
  TodayEarnings
    <$> arbitraryReducedMaybe n -- todayEarningsActualEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsConsensusEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsEstimatedEps :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsAnnounceTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- todayEarningsNumberOfEstimates :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsEpsSurpriseDollar :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsEpsReportDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- todayEarningsFiscalPeriod :: Maybe Text
    <*> arbitraryReducedMaybe n -- todayEarningsFiscalEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- todayEarningsYearAgo :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsYearAgoChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsEstimatedChangePercent :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsSymbolId :: Maybe Double
    <*> arbitraryReducedMaybe n -- todayEarningsSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- todayEarningsQuote :: Maybe Quote
    <*> arbitraryReducedMaybe n -- todayEarningsHeadline :: Maybe Text

instance Arbitrary TodayEarningsAllOf where
  arbitrary = sized genTodayEarningsAllOf

genTodayEarningsAllOf :: Int -> Gen TodayEarningsAllOf
genTodayEarningsAllOf n =
  TodayEarningsAllOf
    <$> arbitraryReducedMaybe n -- todayEarningsAllOfSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- todayEarningsAllOfQuote :: Maybe Quote
    <*> arbitraryReducedMaybe n -- todayEarningsAllOfHeadline :: Maybe Text

instance Arbitrary Trade where
  arbitrary = sized genTrade

genTrade :: Int -> Gen Trade
genTrade n =
  Trade
    <$> arbitraryReducedMaybe n -- tradePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- tradeSize :: Maybe Double
    <*> arbitraryReducedMaybe n -- tradeTradeId :: Maybe Double
    <*> arbitraryReducedMaybe n -- tradeIsIso :: Maybe Bool
    <*> arbitraryReducedMaybe n -- tradeIsOddLot :: Maybe Bool
    <*> arbitraryReducedMaybe n -- tradeIsOutsideRegularHours :: Maybe Bool
    <*> arbitraryReducedMaybe n -- tradeIsSinglePriceCross :: Maybe Bool
    <*> arbitraryReducedMaybe n -- tradeIsTradeThroughExempt :: Maybe Bool
    <*> arbitraryReducedMaybe n -- tradeTimestamp :: Maybe Double




instance Arbitrary E'AuctionType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CalculationPrice where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Flag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Format where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Format2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'IssueType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'LatestSource where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ListType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Period where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PriceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Qualified where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Range where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Range2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Reason where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SecurityEvent where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SystemEvent where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelList where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Sector where
  arbitrary = arbitraryBoundedEnum

