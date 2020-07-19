{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Data.Typeable                  ( Proxy(..) )
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           PropMime
import           Instances                      ( )

import           IEXHaskellSDK.Model
import           IEXHaskellSDK.MimeTypes

main :: IO ()
main = hspec $ modifyMaxSize (const 10) $ do
  describe "JSON instances" $ do
    pure ()
    propMimeEq MimeJSON (Proxy :: Proxy Article)
    propMimeEq MimeJSON (Proxy :: Proxy Auction)
    propMimeEq MimeJSON (Proxy :: Proxy Chart1d)
    propMimeEq MimeJSON (Proxy :: Proxy ChartAll)
    propMimeEq MimeJSON (Proxy :: Proxy ChartNot1d)
    propMimeEq MimeJSON (Proxy :: Proxy CorporateAction)
    propMimeEq MimeJSON (Proxy :: Proxy DividendCoreData)
    propMimeEq MimeJSON (Proxy :: Proxy Earnings)
    propMimeEq MimeJSON (Proxy :: Proxy Financials)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepBookGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StatsHistoricalDailyGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StatsIntradayGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StatsRecentGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StatsRecordsGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy MarketGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy RefDataSymbolsGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolBookGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolChartDynamicGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolCompanyGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolDelayedQuoteGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponseAuction)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponseOpHaltStatus)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponseSecurityEvent)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponseSsrStatus)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponseTradeBreaks)
    propMimeEq MimeJSON (Proxy :: Proxy DeepGetResponseTradingStatus)
    propMimeEq MimeJSON (Proxy :: Proxy HistGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolDividendsRangeGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolEarningsGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolEffectiveSpreadGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolFinancialsGetResponse)
    propMimeEq MimeJSON
               (Proxy :: Proxy StockMarketThresholdSecuritiesGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockMarketUpcomingIposGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolStatsGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolLargestTradesGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolLogoGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolRelevantGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy TopsLastGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockMarketSectorPerformanceGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StocksSymbolSplitsRangeGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockMarketTodayEarningsGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy StockSymbolVolumeByVenueGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepOfficialPriceGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepOpHaltStatusGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepSecurityEventGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepSystemEventGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy TopsGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy DeepTradingStatusGetResponse)
    propMimeEq MimeJSON (Proxy :: Proxy IpoRawData)
    propMimeEq MimeJSON (Proxy :: Proxy IpoViewData)
    propMimeEq MimeJSON (Proxy :: Proxy ListedSymbolData)
    propMimeEq MimeJSON (Proxy :: Proxy ModelList)
    propMimeEq MimeJSON (Proxy :: Proxy OhlcData)
    propMimeEq MimeJSON (Proxy :: Proxy OhlcDataClose)
    propMimeEq MimeJSON (Proxy :: Proxy OhlcDataOpen)
    propMimeEq MimeJSON (Proxy :: Proxy Order)
    propMimeEq MimeJSON (Proxy :: Proxy PriceData)
    propMimeEq MimeJSON (Proxy :: Proxy Quote)
    propMimeEq MimeJSON (Proxy :: Proxy RecordedValue)
    propMimeEq MimeJSON (Proxy :: Proxy Sector)
    propMimeEq MimeJSON (Proxy :: Proxy ShortInterest)
    propMimeEq MimeJSON (Proxy :: Proxy Stats)
    propMimeEq MimeJSON (Proxy :: Proxy SystemEvent)
    propMimeEq MimeJSON (Proxy :: Proxy TimestampedValue)
    propMimeEq MimeJSON (Proxy :: Proxy TodayEarnings)
    propMimeEq MimeJSON (Proxy :: Proxy TodayEarningsAllOf)
    propMimeEq MimeJSON (Proxy :: Proxy Trade)
