{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import IEXHaskellSDK.Model
import IEXHaskellSDK.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
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
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse200)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20010)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20011)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20012)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20013)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20014)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20015)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20016)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20017)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20018)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20019)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001Auction)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001OpHaltStatus)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001SecurityEvent)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001SsrStatus)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001TradeBreaks)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001TradingStatus)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2002)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20020)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20021)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20022)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20023)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20024)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20025)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20026)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20027)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20028)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20029)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2003)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20030)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20031)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20032)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20033)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2004)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2005)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2006)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2007)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2008)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2009)
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
      
