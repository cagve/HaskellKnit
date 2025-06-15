{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

import Servant
import Network.Wai.Handler.Warp (run)
import Servant.Multipart
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Directory (removeFile, doesFileExist, createDirectoryIfMissing)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.Aeson (ToJSON)
import Parser
import Explain

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
        "patterns" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] UploadResponse
  :<|>  "patterns" :> Capture "filename" String :> Get '[JSON] PatternResponse
  :<|>  "delete" :> Capture "filename" String :> Delete '[JSON] DeleteResponse

server :: Server API
server = uploadHandler :<|> patternHandler :<|> deleteHandler

uploadHandler :: MultipartData Mem -> Handler UploadResponse
uploadHandler multipartData = do
  let uploadedFiles = files multipartData
  case uploadedFiles of
    [] -> return $ UploadResponse False "No file uploaded"
    (file:_) -> do
      let fileName = fdFileName file
          fileContent = fdPayload file
          filePath = uploadDir </> T.unpack fileName
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



-- Aplicación WAI
app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  putStrLn "Servidor ejecutándose en http://localhost:8080"
  run 8080 (serve (Proxy :: Proxy API) server)

