{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Servant
import Codec.Picture
import Network.Wai.Handler.Warp (run)
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
import Explain
import Parser
import Graph
import qualified Data.ByteString.Char8 as BS
import Parser (isColorPattern)


uploadDir = "patterns"

data ImageRequest = ImageRequest
  { patternStr :: Text,
    colorList ::  [RGB]
  } deriving (Generic, Show)
instance FromJSON ImageRequest

data ImageResponse = ImageResponse
  { imageSuccess :: Bool
  , imageMsg     :: Text
  , imageUrl :: Maybe Text
  , colorsNumber :: Int
  } deriving (Generic, Show)
instance ToJSON ImageResponse



instance FromMultipart Mem ImageRequest where
  fromMultipart multipart = do
    pat <- lookupInput "patternStr" multipart
    colorJson <- lookupInput "colorList" multipart
    case eitherDecode (BS.fromStrict $ TE.encodeUtf8 colorJson) of
      Left err -> Left ("Error al parsear colorListJson: " ++ err)
      Right colorList -> Right $ ImageRequest pat colorList


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
  , explanationExprMsg :: Maybe Text
  , explanationClusterMsg :: Maybe Text
  , pattern :: Maybe [[Text]]
  , patternErrMsg :: Maybe Text
  , colorPattern :: Bool
  } deriving (Generic, Show)

instance ToJSON PatternResponse

-- Helpers para construir la respuesta
errorResponse :: Text -> PatternResponse
errorResponse errMsg = PatternResponse
  { patternSuccess = False
  , explanationExprMsg = Nothing
  , explanationClusterMsg = Nothing
  , pattern = Nothing
  , patternErrMsg = Just errMsg
  , colorPattern  = False
  }

successResponse :: Maybe Text -> Maybe Text -> Maybe [[Text]] -> Bool -> PatternResponse
successResponse exprMsg clusterMsg pat color = PatternResponse
  { patternSuccess = True
  , explanationExprMsg = exprMsg
  , explanationClusterMsg = clusterMsg
  , pattern = pat
  , patternErrMsg = Nothing
  , colorPattern = color
  }


-- Define la API
type API =
        "patterns"  :> Get '[JSON] [T.Text]
  :<|>  "patterns"  :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] UploadResponse
  :<|>  "patterns"  :> Capture "filename" String :> Get '[JSON] PatternResponse
  :<|>  "delete"    :> Capture "filename" String :> Delete '[JSON] DeleteResponse
  :<|>  "image"     :> MultipartForm Mem ImageRequest :> Post '[JSON] ImageResponse

server :: Server API
server = listPatternsHandler:<|> uploadHandler :<|> patternHandler :<|> deleteHandler :<|> imageHandler

imageHandler :: ImageRequest -> Handler ImageResponse
imageHandler req = do
  let fileName   = T.unpack (patternStr req)
      imgPath    = "img/" ++ fileName ++ ".png"
      imgBase64     = T.pack imgPath
  maybeResult <- liftIO $ processFile (T.unpack $ patternStr req)
  case maybeResult of
    Nothing -> return ImageResponse
      { imageSuccess = False
      , imageMsg = "Error procesando el patrón."
      , imageUrl = Nothing
      , colorsNumber = 0
      }
    Just (_pattern, evaluated) -> do
      let colorsRGB = map convertRGBintoPixelRGBA8 (colorList req)
      case drawColorPattern 90 1 colorsRGB evaluated of
          Left err -> return ImageResponse
            { imageSuccess = False
            , imageMsg = T.pack err
            , imageUrl = Nothing
            , colorsNumber = 0
            }
          Right colorsImg -> do
            let imgData = encodePng colorsImg
                base64Img = TL.toStrict $ TLE.decodeUtf8 $ B64.encode imgData
            return ImageResponse
              { imageSuccess = True
              , imageMsg = "Imagen creada con éxito."
              , imageUrl = Just base64Img
              , colorsNumber = getNumberOfColors evaluated
              }


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
    then
      return $ errorResponse "Archivo no encontrado"
    else do
      parsed <- liftIO $ parsePatternFile filePath
      case parsed of
        Left err -> return $ errorResponse (T.pack $ show err)
        Right pattern ->
          case runEval 40 pattern of
            Left err -> return $ errorResponse (T.pack $ show err)
            Right evaluated -> do
              let patternEvaluated = convertPattern evaluated
                  explanationExpr = explainExprPattern pattern
                  explanationCluster = explainClusterPattern evaluated
                  colorBool = isColorPattern evaluated
              return $ successResponse
                (Just $ T.pack explanationExpr)
                (Just $ T.pack explanationCluster)
                (Just patternEvaluated)
                colorBool

processFile :: String -> IO (Maybe (Pattern, [[Stitch]]))
processFile filename = do
  let filePath = uploadDir </> filename
  exists <- liftIO $ doesFileExist filePath
  if not exists
    then
      return $ Nothing 
    else do
      parsed <- liftIO $ parsePatternFile filePath
      case parsed of
        Left err -> return $ Nothing
        Right pattern ->
          case runEval 40 pattern of
            Left err -> return $ Nothing
            Right evaluated -> do
              return $ Just (pattern, evaluated)


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
