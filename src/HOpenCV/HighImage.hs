{- |
Modulo abstracto para manejo de funciones de la libreria HighGUI del core de OpenCV
-}
module HOpenCV.HighImage where

import HOpenCV.OpenCV

type Image = IplImage

-- | Function para crear una captura desde la camara
createCameraCaptureCV :: Int -> IO Capture
createCameraCaptureCV i = do
    r <- createCameraCapture i
    return (runErr r)

-- | Function para crear una captura desde archivo 
createFileCaptureCV :: String -> IO Capture
createFileCaptureCV str = do
    r <- createFileCapture str
    return (runErr r)

-- | Function para obtener frames
queryFrameCV :: Capture -> IO IplImage
queryFrameCV cap = do
    r <- queryFrame cap
    return (runErr r)

-- | Function que espera una tecla
waitKeyCV :: Int -> IO Int
waitKeyCV k = do
    r <- waitKey k
    return (runErr r)

-- | Function para cargar una imagen
loadImageCV :: String -> LoadImageColor -> IO IplImage
loadImageCV n i = do
    r <- loadImage n i
    return (runErr r)

-- | Function para guardar una imagen
saveImageCV :: String -> IplImage -> IO Int
saveImageCV name img = do
    r <- saveImage name img
    return (runErr r)

-- | Function para trackear la posición
getTrackbarPosCV :: String -> String -> IO Int
getTrackbarPosCV s1 s2 = do
    r <- getTrackbarPos s1 s2
    return (runErr r)


























