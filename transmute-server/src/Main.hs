{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
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
import qualified Data.Map as Map
import           Control.Concurrent.MVar

data FileFormat = PDF
                | PNG
                | JPG
                | Extension String

data ConversionResult = ConversionSuccess FileFormat FileFormat ReceiveURL
                      | ConversionFailure String

data ServerState = ServerState {
    counter :: Int,
    filenames :: Map.Map Int String
  }

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
      "url" .= ("http://www.filetransmute.com/receive/" <> url)
    ]

instance ToJSON FileFormat where
  toJSON PDF = "pdf"
  toJSON PNG = "png"
  toJSON JPG = "jpg"
  toJSON _ = "unknown"

main :: IO ()
main = do
  var <- newMVar $ ServerState 0 Map.empty
  quickHttpServe $ site var

site :: MVar ServerState -> Snap ()
site var =
  ifTop (serveFile "static/html/index.html")      <|>
  route [("convert", method POST $ convert var),
         ("feedback", method POST feedback),
         ("receive/:id", serveReceive var)]       <|>
  dir "static" (serveDirectory "static")

serveReceive :: MVar ServerState -> Snap ()
serveReceive var = do
  filenames <- liftIO $ filenames <$> readMVar var
  receiveName <- read <$> Char.unpack <$> fromJust <$> getParam "id"
  let Just filename = Map.lookup receiveName filenames
  Char.pack <$> liftIO (readFile $ "./receive/" ++ show receiveName) >>= writeBS
  modifyResponse $ setContentType "application/pdf"
  modifyResponse $ setHeader "Content-disposition" $ "attachment; filename=" <> Char.pack filename

convert :: MVar ServerState -> Snap ()
convert var = 
  handleFileUploads "./temp" defaultUploadPolicy (const $ allowWithMaximumSize 1000000) $ \parts -> do
    result <-
      case parts of
        [(PartInfo {partFileName = Just filename}, Right tempfile)] -> do
          from <- getPostParam "from"
          to <- getPostParam "to"

          if all isJust [from, to]
          then
            fromJust $ handleConversion var <$> (fmt <$> from) <*> (fmt <$> to) <*> return (Char.unpack filename, tempfile)
          else
            conversionFailure "/convert POST must have parameters: 'from', 'to', and 'file'."
        _ -> conversionFailure "Could not read file; or too many files."

    writeLBS $ encode result
    modifyResponse $ setContentType "application/json"

  where
    fmt :: ByteString -> FileFormat
    fmt "pdf" = PDF
    fmt "png" = PNG
    fmt "jpg" = JPG
    fmt ext = Extension $ Char.unpack ext

handleConversion :: MVar ServerState -> FileFormat -> FileFormat -> (FilePath, FilePath) -> Snap ConversionResult
handleConversion _ (Extension ext) _ _ = conversionFailure $ "Unknown format: " <> ext
handleConversion _ _ (Extension ext) _ = conversionFailure $ "Unknown format: " <> ext
handleConversion var from to (filepath, tempfile) = do
  receiveName <- liftIO $ modifyMVar var $ \state ->
    return (state {
      counter = counter state + 1,
      filenames = Map.insert (counter state + 1) filepath (filenames state)
    }, show $ counter state + 1)
  liftIO $ writeFile ("./receive/" ++ receiveName) "Hello!"
  return $ ConversionSuccess from to (Receive receiveName)

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
        "<center><b>Thank you for submitting feedback!</b></center><br/>",
        "<hr/>",
        "<p><b>Feedback:</b> ", pack $ Char.unpack content, "</p>",
        "<hr/>",
        "<p>Feel free to reply to this email if you'd ",
        "like to submit more feedback or have any questions.</p>",
        "<p>– Andrew</br><br/><i>andrew@filetransmute.com</i></p>"
      ]
