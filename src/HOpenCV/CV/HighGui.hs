{-# LINE 1 "HOpenCV/CV/HighGui.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "HOpenCV/CV/HighGui.hsc" #-}

{- |
While OpenCV was designed for use in full-scale applications and can be used
within functionally rich UI frameworks (such as Qt*, WinForms*, or Cocoa*) or
without any UI at all, sometimes there it is required to try functionality
quickly and visualize the results. This is what the "OpenCV.HighGui" module has
been designed for.

It provides easy interface to:

 * Create and manipulate windows that can display images and “remember” their
   content (no need to handle repaint events from OS).

 * Add trackbars to the windows, handle simple mouse events as well as keyboard
   commands.
-}
module HOpenCV.CV.HighGui where

import Control.Monad
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable

import HOpenCV.CV.CxCore
import HOpenCV.CV.Util
import HOpenCV.CV.Error

-- * List of functions in the module:

{-# LINE 19 "HOpenCV/CV/HighGui.hsc" #-}

------------------------------------------------

foreign import ccall unsafe "opencv/highgui.h cvConvertImage"
  c_cvConvertImage :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CInt -> IO ()

-- | Function convertImage
convertImage :: IplImage -> IplImage -> Int -> IO ()
convertImage src dst flags
  = withForeignPtr2 src dst
     $ \s d -> c_cvConvertImage s d
                                (fromIntegral flags)

------------------------------------------------
-- | Definition Type Capture
data Priv_CvCapture
type Capture = ForeignPtr Priv_CvCapture


foreign import ccall unsafe "opencv/highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr Priv_CvCapture)

-- | self-documenting camera specification
pickAnyCam :: Int
pickAnyCam = -1

-- | self-documenting camera specification
cam :: Int -> Int
cam = id 


-- |Open a capture stream from a connected camera. The parameter is
-- the index of the camera to be used, or 'Nothing' if it does not
-- matter what camera is used. The returned action may be used to
-- query for the next available frame.
createCameraCapture :: Int -> IO (ErrCV Capture)
createCameraCapture x
  = do p <- mycheckPtr_2 $ c_cvCreateCameraCapture . fromIntegral $ x
       case p of
            (Return p') -> do
                            ptr <- newForeignPtr cp_release_capture p'
                            return (Return ptr)
            (Raise s) -> return( throwErrorCV ("Failed to create camera (" ++ s ++ ")") )


foreign import ccall unsafe "opencv/highgui.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr Priv_CvCapture)

createFileCapture :: String -> IO (ErrCV Capture)
createFileCapture filename
  = do c <- mycheckPtr_2 $ withCString filename f
       case c of
            (Return c') -> do
                            ptr <- newForeignPtr cp_release_capture c'
                            return (Return ptr)
            (Raise s) -> return( throwErrorCV ("Failed to capture from file: '" ++ filename ++ "' (" ++ s ++ ")") )
    where f filenameC = c_cvCreateFileCapture filenameC

foreign import ccall unsafe "HOpenCV_wrap.h &release_capture" 
  cp_release_capture  :: FunPtr (Ptr Priv_CvCapture -> IO ())

foreign import ccall unsafe "opencv/highgui.h cvReleaseCapture"
  cv_release_capture  ::  Ptr Priv_CvCapture -> IO ()

release_capture :: Capture -> IO ()
release_capture cap = withForeignPtr cap $ \c -> cv_release_capture c
  
foreign import ccall unsafe "opencv/highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr Priv_CvCapture -> IO (Ptr Priv_IplImage)

-- |If 'cvQueryFrame' returns 'Nothing', try rewinding the video and
-- querying again. If it still fails, raise an error. When a non-null
-- frame is obtained, return it.
queryFrame :: Capture -> IO (ErrCV IplImage)
queryFrame cap
  = do i <- withForeignPtr cap $ \c -> mycheckPtr_2 $ c_cvQueryFrame c
       case i of
            (Return i') -> do
                               fp <- newForeignPtr_ i' -- no free! OpenCV demands queryFrame results not be freed by user.
                               return (Return fp)
            (Raise s) -> return( throwErrorCV ("Failed to query frame from camera (" ++ s ++ ")") )

-------------------------------------------------
-- * Windows

foreign import ccall unsafe "opencv/highgui.h cvNamedWindow"
  cvNamedWindow :: CString -> CInt -> IO CInt

type AutoSize = Bool

-- | self-documenting window sizing specification
autoSize :: AutoSize
autoSize   = True

namedWindow :: String -> AutoSize -> IO ()
namedWindow s a
  = withCString s $ \cs ->
      do _ <- cvNamedWindow cs (fromIntegral $ fromEnum a)
         return ()

foreign import ccall unsafe "opencv/highgui.h cvDestroyWindow"
  cvDestroyWindow :: CString -> IO ()

destroyWindow :: String -> IO ()
destroyWindow wId
  = withCString wId cvDestroyWindow

foreign import ccall unsafe "opencv/highgui.h cvShowImage"
  cvShowImage :: CString -> Ptr Priv_IplImage -> IO ()

showImage :: String -> IplImage -> IO ()
showImage wId p
 = withCString wId $ \w ->
    withForeignPtr p $ cvShowImage w

foreign import ccall unsafe "opencv/highgui.h cvWaitKey"
  cvWaitKey :: CInt -> IO CInt

waitKey :: Int -> IO (ErrCV Int)
waitKey milliSecs
  = do i <- cvWaitKey $ fromIntegral milliSecs
       if i == (-1)
         then return (throwErrorCV ("No Key press..."))
         else return (Return (fromIntegral i))

-- |Determine the color model of an image loaded from a file.
newtype LoadImageColor = LoadImageColor { unLoadImageColor :: CInt }

loadImageColor      :: LoadImageColor
loadImageColor      = LoadImageColor 1
loadImageGrayscale  :: LoadImageColor
loadImageGrayscale  = LoadImageColor 0
loadImageUnchanged  :: LoadImageColor
loadImageUnchanged  = LoadImageColor (-1)

{-# LINE 135 "HOpenCV/CV/HighGui.hsc" #-}

foreign import ccall unsafe "opencv/highgui.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr Priv_IplImage)

loadImage :: String -> LoadImageColor -> IO (ErrCV IplImage)
loadImage filename (LoadImageColor color) 
  = do i <- mycheckPtr_2 $ withCString filename
            $ \fn -> c_cvLoadImage fn color
       case i of
        (Return x) -> do 
                        fp <- newForeignPtr cvFree x
                        return (Return fp)
        (Raise s) -> return (throwErrorCV ("Failed to load from file: '" ++ filename ++ "' (" ++ s ++ ")") )


foreign import ccall unsafe "opencv/highgui.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr Priv_IplImage -> IO CInt

saveImage :: String -> IplImage -> IO (ErrCV Int)
saveImage filename image = withCString filename f
  where
    f filenameC = do
      ret <- withForeignPtr image $ \i -> 
             c_cvSaveImage filenameC i
      if (ret == 0) 
        then return (throwErrorCV ("Failed to save to file: '" ++ filename ++ "'") )
        else return (Return (fromIntegral ret))

------------------------------------------------
-- * Trackbar

foreign import ccall unsafe "HOpenCV_Wrap.h wrap_createTrackbar"
  wrap_createTrackbar :: CString -> CString -> Ptr CInt -> CInt -> IO ()

createTrackbar :: String -> String -> Maybe Int -> Int -> IO ()
createTrackbar trackbarName winName startPosition maxValue
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
    alloca                   $ \sp ->
      do maybeToPtr sp startPosition
         wrap_createTrackbar tb wn sp (fromIntegral maxValue)
 where
  maybeToPtr mem (Just i) = poke mem (fromIntegral i)
  maybeToPtr mem Nothing  = poke mem (fromIntegral 0)

foreign import ccall unsafe "opencv/highgui.h cvGetTrackbarPos"
  cvGetTrackbarPos :: CString -> CString -> IO CInt

getTrackbarPos :: String -> String -> IO (ErrCV Int)
getTrackbarPos trackbarName winName
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
      do i <- cvGetTrackbarPos tb wn
         if (i == 0) 
            then return (throwErrorCV ("Failed to getTrackbarPos to: '" ++ trackbarName ++ "' in winName: '"++ winName ++ "'") )
            else return (Return (fromIntegral i))         


foreign import ccall unsafe "opencv/highgui.h cvSetTrackbarPos"
  cvSetTrackbarPos :: CString -> CString -> CInt -> IO ()

setTrackbarPos :: String -> String -> Int -> IO ()
setTrackbarPos trackbarName winName pos
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
      cvSetTrackbarPos tb wn (fromIntegral pos)
