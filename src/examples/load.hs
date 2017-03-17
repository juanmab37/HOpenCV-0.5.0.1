module Main where

import HOpenCV.OpenCV
import HOpenCV.HighImage

import Control.Monad


main :: IO ()
main = do 
        putStrLn "Running..."
        let win = "win"
        namedWindow win autoSize
        img <- loadImageCV "data/Lenna.png" loadImageColor
        showImage win img
        waitKey 5000
        saveImage "new.png" img
        destroyWindow win
       
      
         
