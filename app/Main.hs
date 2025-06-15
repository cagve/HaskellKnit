{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Servant
import Codec.Picture (DynamicImage (..), Image (..), PixelRGBA8 (..), savePngImage)
import Network.Wai.Handler.Warp (run)
import Servant.Multipart
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Directory (removeFile, doesFileExist, createDirectoryIfMissing, listDirectory)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.Aeson (FromJSON, ToJSON)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Explain
import Parser
import Graph
import qualified Data.ByteString.Char8 as BS


uploadDir = "patterns"

data ImageRequest = ImageRequest
  { patternStr :: Text,
    colors :: [Text]
  } deriving (Generic, Show)
instance FromJSON ImageRequest

data ImageResponse = ImageResponse
  { imageSuccess :: Bool
  , imageMsg     :: Text
  } deriving (Generic, Show)
instance ToJSON ImageResponse



instance FromMultipart Mem ImageRequest where
  fromMultipart multipart = do
    pat <- lookupInput "patternStr" multipart
    colorStr <- lookupInput "colors" multipart
    let colorList = T.splitOn "," colorStr
    return $ ImageRequest pat colorList



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
  }

successResponse :: Maybe Text -> Maybe Text -> Maybe [[Text]] -> PatternResponse
successResponse exprMsg clusterMsg pat = PatternResponse
  { patternSuccess = True
  , explanationExprMsg = exprMsg
  , explanationClusterMsg = clusterMsg
  , pattern = pat
  , patternErrMsg = Nothing
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
  let colors = [ PixelRGBA8 180 215 242 255
               , PixelRGBA8 238 28 37 255
               , PixelRGBA8 255 202 10 255
               ]
  maybeResult <- liftIO $ processFile (T.unpack $ patternStr req)
  imageMsg <- case maybeResult of
    Nothing -> return "Error procesando el patrón."
    Just (_pattern, evaluated) -> do
      let colorsImg = drawColorPattern 90 2 colors evaluated
      liftIO $ savePngImage "resultado.png" (ImageRGBA8 colorsImg)
      return "Imagen creada con éxito."
  return ImageResponse
    { imageSuccess = maybeResult /= Nothing
    , imageMsg = imageMsg
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
              return $ successResponse
                (Just $ T.pack explanationExpr)
                (Just $ T.pack explanationCluster)
                (Just patternEvaluated)


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
        { corsOrigins = Nothing -- permite todos los orígenes
          , corsMethods = [BS.pack "GET", BS.pack "POST", BS.pack "DELETE"]
        }
      corsMiddleware = cors (const $ Just corsPolicy)
  putStrLn "Servidor ejecutándose en http://localhost:8080"
  run 8080 $ corsMiddleware app
