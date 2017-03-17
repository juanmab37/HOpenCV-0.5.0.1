{- |
Modulo abstracto para manejo de funciones de la libreria Utils del core de OpenCV
-}
module HOpenCV.HUtil where

import HOpenCV.OpenCV
import HOpenCV.CV.Util
import Foreign

-- | Function de chequeo de punteros
mycheckPtr :: IO (Ptr a) -> IO (Ptr a)
mycheckPtr x = do
    r <- mycheckPtr_2 x
    return (runErr r)
























