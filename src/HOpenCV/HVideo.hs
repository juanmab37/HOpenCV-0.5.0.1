{- |
Modulo abstracto para manejo de funciones de la libreria de Video del core de OpenCV
-}
module HOpenCV.HVideo where

import HOpenCV.OpenCV

-- | Function para crear un escritor de video
createVideoWriterCV :: String -> FourCC -> Double -> CvSize -> IO CvVideoWriter
createVideoWriterCV file codec fps size = do
    r <- createVideoWriter file codec fps size
    return (runErr r)

-- | Function para escribir un frame
writeFrameCV :: CvVideoWriter -> IplImage -> IO Int
writeFrameCV vw im = do
    r <- writeFrame vw im
    return (runErr r)























