{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.Maybe (isJust, fromJust, maybeToList)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char
import           Data.Aeson
import           Data.Monoid
import           Network.Api.Postmark
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text (Text, pack)

data FileFormat = PDF
                | PNG
                | JPG
                | Extension String

data ConversionResult = ConversionSuccess FileFormat FileFormat ReceiveURL
                      | ConversionFailure String

newtype ReceiveURL = Receive String

postmarkSettings :: PostmarkSettings
postmarkSettings = postmarkHttps "07b7320d-98e9-4cc6-a998-afd10c7fd9fb"

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
feedback = do
  address <- getPostParam "email"
  message <- getPostParam "message"
  case message of
    Nothing      -> failFeedback
    Just content ->
      let to = "andrew@filetransmute.com" : maybeToList address in
        sendFeedback to content
  where
    failFeedback :: Snap ()
    failFeedback = undefined

    sendFeedback :: [ByteString] -> ByteString -> Snap ()
    sendFeedback to content = 
      void $ liftIO $ request postmarkSettings $ email $ defaultEmail {
          emailFrom = "andrew@filetransmute.com",
          emailTo = map (pack . Char.unpack) to,
          emailSubject = "File Transmute Feedback",
          emailHtml =  Just $ formatEmail content
        }

    formatEmail :: ByteString -> Text
    formatEmail content = mconcat [
        "<b>Thank you for submitting feedback!</b><br/>",
        "<hr/>",
        "<p><b>Feedback:</b> ", pack $ Char.unpack content, "</p>",
        "<hr/>",
        "<p>Feel free to reply to this email if you'd ",
        "like to submit more feedback or have any questions.</p>",
        "<p>– Andrew</br><br/><i>andrew@filetransmute.com</i></p>"
      ]
