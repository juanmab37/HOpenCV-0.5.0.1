module Main where

import HOpenCV.OpenCV
import HOpenCV.HighImage
import HOpenCV.HCxCore
import HOpenCV.HVideo

import Control.Monad
import Data.Maybe (isNothing) 

showFrames :: String -> IplImage -> Capture -> IO ()
showFrames win target cap  = do
  let step 0 = return ()
      step n = do
        frame <- queryFrameCV cap
        convertImage frame target 0
        canny target target 30 190 3
        showImage win target
        k <- waitKey 5
        case k of
            (Return c) -> putStr $ "\r" ++ show n ++ "\t\t frames before exiting...\r"
            (Raise s) ->  step (n - 1)
  step 1000 

main :: IO ()
main = do
  putStrLn "Running..."
  let win = "win"
  namedWindow win autoSize
  cap    <- createCameraCaptureCV 0
  frame  <- queryFrameCV cap
  size   <- getSizeCV frame
  print ("Size (" ++ show (sizeWidth size) ++ "," ++ show (sizeHeight size) ++ ")\n")
  target <- createImageCV size iplDepth8u 1
  showFrames win target cap
  putStrLn "Fin"
   
         
