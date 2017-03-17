module Main where

import HOpenCV.OpenCV
import HOpenCV.HighImage
import HOpenCV.HCxCore
import HOpenCV.HUtil
import HOpenCV.HVideo
import HOpenCV.ImageProcessors

import qualified Control.Processor as Processor
import Control.Processor((--<))

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category

resizer :: ImageProcessor
resizer = resizeIP 320 240 CV_INTER_LINEAR

edges :: ImageProcessor
edges = cannyIP 30 190 3

faceDetect :: Processor.IOProcessor Image [CvRect]
faceDetect = haarDetect "haarcascade_frontalface_alt.xml" 1.1 3 haarFlagNone (CvSize 20 20)
  
captureDev :: ImageSource
--captureDev = videoFile "/tmp/video.flv" -- Many formats are supported, not just flv (FFMPEG-based, normally).

-- If you have a webcam, uncomment this, and comment the other definition.
captureDev = camera 0

-- Shows the camera output in two windows (same images in both).
main :: IO ()
main = runTillKeyPressed ((camera 0) --< (window 0 *** window 1))
