module Graph  where

import Codec.Picture (readImage, convertRGBA8, savePngImage, Image(..), PixelRGBA8, DynamicImage(..))
import Codec.Picture.Types (pixelAt, generateImage, PixelRGBA8(..))
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Parser
import Print

recFolder :: String
recFolder = "rec"

-- Carga una imagen PNG y la convierte a RGBA8
loadPng :: FilePath -> IO (Image PixelRGBA8)
loadPng path = do
  eimg <- readImage path
  case eimg of
    Left err -> error $ "Error cargando imagen: " ++ err
    Right dynImg -> return $ convertRGBA8 dynImg


-- Convierte un punto en una imagen
stitchToFilename :: Stitch -> FilePath
stitchToFilename st = case st of
  K -> recFolder </> "k.png"
  P  -> recFolder </> "p.png"
  YO  -> recFolder </> "yo.png"
  SSK  -> recFolder </> "ssk.png"
  S2KP2  -> recFolder </> "s2kp2.png"
  KTOG 2-> recFolder </> "k2tog.png"
  M1R  -> recFolder </> "mr.png"
  M1L  -> recFolder </> "ml.png"
  CO  -> recFolder </> "co.png"
  O  -> recFolder </> "pass.png"
  WT  -> recFolder </> "wt.png"
  _ -> recFolder </> "noname.jpg"

    
-- combineVertical ::  Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
-- combineVertical img1 img2 = 
--   generateImage pixelFunc newWidth newHeight
--   where
--     w1 = imageWidth img1
--     h1 = imageHeight img1
--     w2 = imageWidth img2
--     h2 = imageHeight img2
--     newWidth = max w1 w2
--     newHeight = h1 + h2
--
--     pixelFunc x y
--       | y < h1 && x < w1 = pixelAt img1 x y
--       | y >= h1 && x < w2 = pixelAt img2 x (y - h1)
--       | otherwise = PixelRGBA8 0 0 0 0 -- transparente

combineVertical :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
combineVertical img1 img2 = generateImage pixelFunc newWidth newHeight
  where
    w1 = imageWidth img1
    h1 = imageHeight img1
    w2 = imageWidth img2
    h2 = imageHeight img2
    newWidth = max w1 w2
    newHeight = h1 + h2

    
    -- CENTER
    center = True
    xOffset1 = if center then (newWidth - w1) `div` 2 else 0
    xOffset2 = if center then (newWidth - w2) `div` 2 else 0

    pixelFunc x y
      | y < h1 && x >= xOffset1 && x < xOffset1 + w1 = pixelAt img1 (x - xOffset1) y
      | y >= h1 && x >= xOffset2 && x < xOffset2 + w2 = pixelAt img2 (x - xOffset2) (y - h1)
      | otherwise = PixelRGBA8 0 0 0 0 -- transparente

-- Combina dos imágenes horizontalmente
combineHorizontal :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
combineHorizontal img1 img2 =
  generateImage pixelFunc newWidth newHeight
  where
    w1 = imageWidth img1
    h1 = imageHeight img1
    w2 = imageWidth img2
    h2 = imageHeight img2
    newWidth = w1 + w2
    newHeight = max h1 h2

    pixelFunc x y
      | x < w1 && y < h1 = pixelAt img1 x y
      | x >= w1 && y < h2 = pixelAt img2 (x - w1) y
      | otherwise = PixelRGBA8 0 0 0 0 -- transparente

-- Combina una lista de imágenes horizontalmente
combineManyHorizontal :: [Image PixelRGBA8] -> Image PixelRGBA8
combineManyHorizontal = foldl1 combineHorizontal

combineManyVertical :: [Image PixelRGBA8] -> Image PixelRGBA8
combineManyVertical = foldl1 combineVertical

-- Nueva función: recibe una lista de rutas y combina esas imágenes
combineImagesFromList :: [FilePath] -> IO (Image PixelRGBA8)
combineImagesFromList paths = do
  images <- mapM loadPng paths
  return $ combineManyHorizontal images

createRowImg :: [Stitch] -> IO (Image PixelRGBA8)
createRowImg row = do
  let files = map stitchToFilename row
  combineImagesFromList files

createPatternImg :: [[Stitch]] -> IO (Image PixelRGBA8)
createPatternImg pattern = do
  let indexedPattern = zip [0..] (reverseList pattern)
      transformedPattern = [ if even i then row else  reverseList row
                           | (i, row) <- indexedPattern ]
  patternRow <- mapM createRowImg transformedPattern  -- [Image PixelRGBA8]
  let combined = combineManyVertical patternRow
  return combined


drawSquare :: Int -> Int -> PixelRGBA8 -> PixelRGBA8 -> Image PixelRGBA8
drawSquare size borderThickness fillColor borderColor = generateImage pixelFunc size size
  where
    pixelFunc x y
      | x < borderThickness || x >= size - borderThickness || y < borderThickness || y >= size - borderThickness = borderColor
      | otherwise = fillColor


-- toma la lista de colores y el Stitch, devuelve fill y border
stitchToColor :: [PixelRGBA8] -> Stitch -> (PixelRGBA8, PixelRGBA8)
stitchToColor colors (C n) =
  let defaultFill = PixelRGBA8 255 255 255 255
      borderColor = PixelRGBA8 0 0 0 255
      fillColor = if n >= 0 && n <= length colors-1
                  then colors !! n
                  else defaultFill
  in (fillColor, borderColor)


drawColorFromStitch :: Int -> Int -> [PixelRGBA8] -> Stitch -> Image PixelRGBA8
drawColorFromStitch size borderThickness colors st =
  let (fillColor, borderColor) = stitchToColor colors st
  in drawSquare size borderThickness fillColor borderColor

drawColorRow :: Int -> Int -> [PixelRGBA8] -> [Stitch] -> Image PixelRGBA8
drawColorRow size borderThickness colors row =
  let squares = map (drawColorFromStitch size borderThickness colors) row
  in combineManyHorizontal squares

drawColorPattern :: Int -> Int -> [PixelRGBA8] -> [[Stitch]] -> Image PixelRGBA8
drawColorPattern size borderThickness colors pattern =
  let patternRow = map (drawColorRow size borderThickness colors) pattern
  in combineManyVertical patternRow
