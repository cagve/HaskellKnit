{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Servant
import Codec.Picture
import Servant.Multipart
import Data.Text (Text)
import Network.Wai.Middleware.Static
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import System.Directory (removeFile, doesFileExist, createDirectoryIfMissing, listDirectory)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64.Lazy as B64
import GHC.Generics (Generic)
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Parser
import Evaluate
import Explain
import qualified Data.ByteString.Char8 as BS

uploadDir = "patterns"




-- Respuesta para upload
data UploadResponse = UploadResponse
  { uploadSuccess :: Bool
  , uploadMsg     :: Text
  } deriving (Generic, Show)

instance ToJSON UploadResponse

data DeleteResponse = DeleteResponse
  { deleteSuccess :: Bool
  , deleteMsg     :: Text
  } deriving (Generic, Show)
instance ToJSON DeleteResponse

data PatternResponse = PatternResponse
  { patternSuccess :: Bool
  , patternStruct :: Maybe Pattern
  , patternCompact :: Maybe [Explanation]
  , patternExplanation :: Maybe [Explanation]
  , patternErrMsg :: Maybe Text
  , colorPattern :: Bool
  } deriving (Generic, Show)
instance ToJSON PatternResponse

-- Helpers para construir la respuesta
errorResponse :: Text -> PatternResponse
errorResponse errMsg = PatternResponse
  { patternSuccess = False
  , patternStruct = Nothing
  , patternCompact = Nothing
  , patternExplanation = Nothing
  , patternErrMsg = Just errMsg
  , colorPattern  = False
  }

successResponse
  :: Maybe Pattern
  -> Maybe [Explanation]
  -> Maybe [Explanation]
  -> Bool                -- colorPattern
  -> PatternResponse
successResponse pat compact explanation color = PatternResponse
  { patternSuccess = True
  , patternStruct = pat
  , patternCompact   = compact
  , patternExplanation   = explanation
  , patternErrMsg = Nothing
  , colorPattern = color
  }

-- Define la API
type API =
        "patterns"  :> Get '[JSON] [T.Text]
  :<|>  "patterns"  :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] UploadResponse
  :<|>  "patterns"  :> Capture "filename" String :> Get '[JSON] PatternResponse
  :<|>  "delete"    :> Capture "filename" String :> Delete '[JSON] DeleteResponse

server :: Server API
server = listPatternsHandler:<|> uploadHandler :<|> patternHandler :<|> deleteHandler



listPatternsHandler :: Handler [T.Text]
listPatternsHandler = do
  files <- liftIO $ do
    createDirectoryIfMissing True uploadDir -- asegúrate que el directorio exista
    listDirectory uploadDir
  return $ map T.pack files


uploadHandler :: MultipartData Mem -> Handler UploadResponse
uploadHandler multipartData = do
  let uploadedFiles = files multipartData
  case uploadedFiles of
    [] -> return $ UploadResponse False "No file uploaded"
    (file:_) -> do
      let fileName = fdFileName file
          fileContent = fdPayload file
          filePath = uploadDir </> T.unpack fileName
      exists <- liftIO $ doesFileExist filePath
      if exists
        then do
            return $ UploadResponse True ("Archivo "<> T.pack filePath <> " ya existe.")
          else
            liftIO $ do
              createDirectoryIfMissing True uploadDir
              BL.writeFile filePath fileContent
              return $ UploadResponse True ("Archivo subido a " <> T.pack filePath)

-- Handler para borrar archivos
deleteHandler :: String -> Handler DeleteResponse
deleteHandler filename = do
  let filePath = uploadDir </> filename
  exists <- liftIO $ doesFileExist filePath
  if exists
    then do
      liftIO $ removeFile filePath
      return $ DeleteResponse True ("Archivo borrado: " <> T.pack filePath)
    else
      return $ DeleteResponse False "Archivo no encontrado"

patternHandler :: String -> Handler PatternResponse
patternHandler filename = do
  let filePath = uploadDir </> filename
  exists <- liftIO $ doesFileExist filePath
  if not exists
    then return $ errorResponse "Archivo no encontrado"
    else do
      result <- liftIO $ parsePatternFile filePath
      case result of
        Left err -> return $ errorResponse (T.pack $ show err)
        Right pattern ->
          case runEval 40 pattern of
            Left err -> return $ errorResponse (T.pack $ show err)
            Right evaluated -> do
              let colorBool = isColorPattern evaluated
                  explanation = explainExprPattern pattern
                  compact = explainCompactPattern pattern
              return $ successResponse (Just pattern) (Just compact) (Just explanation) colorBool





-- Aplicación WAI
app :: Application
app = serve (Proxy :: Proxy API) server
main :: IO ()
main = do
  let corsPolicy = simpleCorsResourcePolicy
        { corsOrigins = Nothing
        , corsMethods = ["GET", "POST", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }
      corsMiddleware = cors (const $ Just corsPolicy)
      staticMiddleware = staticPolicy (addBase "img")  -- aquí sirve archivos desde /img

  putStrLn "Servidor ejecutándose en http://localhost:8080"
  run 8080 $ corsMiddleware $ staticMiddleware app
  -- test

test :: IO()
test = do
  let filename = "patterns/test.knit" -- archivo por defecto si no hay args
      gauge  = Gauge (Measure 10 10) (StitchTension 26 40)
  putStrLn $ " Usando archivo: "   ++ filename
  result <- parsePatternFile filename
  case result of
    Left err -> putStrLn $ "Error  parseando archivo: " ++ show err
    Right pattern -> do
      putStrLn "\nAST: "
      putStrLn $ show pattern
      putStrLn ""
      case runEval 40 pattern of
        Left err -> putStrLn $ " Error in pattern.:  " ++ err
        Right evaluated -> do
          putStrLn "\nPatrón: "
          putStrLn $ show result
          -- putStrLn $ show evaluated
          putStrLn $ show (explainCompactPattern pattern)
