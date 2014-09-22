{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.Maybe (isJust, fromJust)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char
import           Data.Aeson
import           Data.Monoid

data FileFormat = PDF
                | PNG
                | JPG
                | Extension String

data ConversionResult = ConversionSuccess FileFormat FileFormat ReceiveURL
                      | ConversionFailure String

newtype ReceiveURL = Receive String

instance ToJSON ConversionResult where
  toJSON (ConversionFailure reason) = object [
      "status" .= ("failure" :: String),
      "error" .= reason
    ]

  toJSON (ConversionSuccess from to (Receive url)) = object [
      "status" .= ("success" :: String),
      "from" .= from,
      "to" .= to,
      "url" .= ("http=//www.filetransmute.com/receive/" <> url)
    ]

instance ToJSON FileFormat where
  toJSON PDF = "pdf"
  toJSON PNG = "png"
  toJSON JPG = "jpg"
  toJSON _ = "unknown"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (serveFile "static/html/index.html")                                   <|>
  route [("convert", method POST convert), ("feedback", method POST feedback)] <|>
  dir "static" (serveDirectory "static")

convert :: Snap ()
convert = do
  --  Acquire parameters.
  from <- getPostParam "from"
  to <- getPostParam "to"
  file <- getPostParam "file"

  result <-
    if all isJust [from, to, file]
    then
      fromJust $ handleConversion <$> (fmt <$> from) <*> (fmt <$> to) <*> file
    else
      conversionFailure "/convert POST must have parameters: 'from', 'to', and 'file'."

  writeLBS $ encode result

  where
    fmt :: ByteString -> FileFormat
    fmt "pdf" = PDF
    fmt "png" = PNG
    fmt "jpg" = JPG
    fmt ext = Extension $ Char.unpack ext

handleConversion :: FileFormat -> FileFormat -> ByteString -> Snap ConversionResult
handleConversion (Extension ext) _ _ = conversionFailure $ "Unknown format: " <> ext
handleConversion _ (Extension ext) _ = conversionFailure $ "Unknown format: " <> ext
handleConversion from to _ =
  return $ ConversionSuccess from to (Receive "F6lmItFxnNM")

conversionFailure :: String -> Snap ConversionResult
conversionFailure = return . ConversionFailure

feedback :: Snap ()
feedback = undefined
