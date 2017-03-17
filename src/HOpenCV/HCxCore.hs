{- |
Modulo abstracto para manejo de funciones de la libreria CxCore del core de OpenCV
-}
module HOpenCV.HCxCore where

import HOpenCV.OpenCV

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Storable

-- | Function para crear un almacenamiento
createMemStorageCV :: Int -> IO MemStorage
createMemStorageCV i = do
    r <- createMemStorage i
    return (runErr r)

-- | Function para crear una imagen
createImageCV :: CvSize -> Depth -> Int -> IO IplImage
createImageCV size dept i = do
    r <- createImage size dept i
    return (runErr r)

-- | Function para clonar una imagen
cloneImageCV :: IplImage -> IO IplImage
cloneImageCV i = do
    r <- cloneImage i
    return (runErr r)

-- | Function para obtener datos de una imagen
getImageDataCV :: IplImage -> IO (Ptr CUChar)
getImageDataCV i = do
    r <- getImageData i
    return (runErr r)

-- | Function para obtener las medidas de una imagen
getSizeCV :: IplImage -> IO CvSize
getSizeCV i = do
    r <- getSize i
    return (runErr r)

-- | Function para obtener la profundidad una imagen
getDepthCV :: IplImage -> IO Depth
getDepthCV i = do
    r <- getDepth i
    return (runErr r)

-- | Function para obtener el numero de canales de una imagen
getNumChannelsCV :: Integral a => IplImage -> IO a
getNumChannelsCV i = do
    r <- getNumChannels i
    return (runErr r)

-- | Function para obtener el paso de ancho una imagen
getWidthStepCV :: IplImage -> IO Int
getWidthStepCV i = do
    r <- getWidthStep i
    return (runErr r)

-- | Function para cargar una imagen de un almacenamiento
loadCV :: String -> MemStorage -> Maybe String -> IO (ForeignPtr a, Maybe String)
loadCV s m ms = do
    r <- load s m ms
    return (runErr r)

seqToPListCV :: CvSeq a -> IO [ForeignPtr a]
seqToPListCV cq = do
    r <- seqToPList cq
    return ([runErr (head r)])

seqToListCV :: Storable a => CvSeq a -> IO [a]
seqToListCV cq = do
    r <- seqToList cq
    return ([runErr (head r)])


























