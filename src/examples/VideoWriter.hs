-- | Save video from an attached webcam to compressed video on disk
-- while also showing it on-screen.
import HOpenCV.OpenCV
import HOpenCV.HighImage
import HOpenCV.HCxCore
import HOpenCV.HVideo
import System.Exit (exitSuccess)

main :: IO ()
main = do cam <- createCameraCaptureCV 0  
          writeImg <- createVideoWriterCV "foo.avi" (toFourCC "XVID") 10 (CvSize 640 480)  
          namedWindow "Video Test" autoSize
          let kb 27 = exitSuccess
              kb  _ = go
              go = do frame <- queryFrameCV cam
                      showImage "Video Test" frame
                      i <- writeFrameCV writeImg frame
                      --print i
                      r <- waitKey 1 
                      case r of
                        (Return r') -> kb r'
                        (Raise s) -> go                        
          go
          releaseVideoWriter writeImg
          release_capture cam 
