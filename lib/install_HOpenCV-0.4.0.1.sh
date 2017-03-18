https://hackage.haskell.org/package/HOpenCV
https://www.haskell.org/cabal/users-guide/developing-packages.html#editing-the-.cabal-file
https://www.openhub.net/p/HOpenCV

--> Download OpenCV 2.0: https://sourceforge.net/projects/opencvlibrary/files/opencv-unix/2.0/

//Install dependecias OpenCV
sudo apt-get -y install build-essential libgtk2.0-dev libjpeg62-dev libtiff5-dev libjasper-dev libopenexr-dev cmake python-dev python-numpy libtbb-dev libeigen2-dev yasm libfaac-dev libopencore-amrnb-dev libopencore-amrwb-dev libtheora-dev libvorbis-dev libxvidcore-dev


The installation procedure at glance:
=====================================
   1. tar -xjf OpenCV-2.0.0.tar.bz2
   2. mkdir opencv.build
   3. cd opencv.build
   4a. cmake [<extra_options>] ../OpenCV-2.0.0 # CMake-way
      or
   4b. ../OpenCV-2.0.0/configure [<extra_options>] # configure-way
   4c. cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D BUILD_ZLIB=ON -D WITH_V4L=ON -D WITH_GSTREAMER=ON -D WITH_OPENEXR=ON -D WITH_UNICAP=ON -D BUILD_PYTHON_SUPPORT=ON -D INSTALL_C_EXAMPLES=ON -D INSTALL_PYTHON_EXAMPLES=ON -D BUILD_EXAMPLES=ON ..
   4d.cmake -DWITH_FFMPEG=OFF .
   5. make -j 2
   6. sudo make install
   7. sudo ldconfig # linux only

//Errores
http://stackoverflow.com/questions/9837555/installing-opencv2-1-on-ubuntu-gives-error
https://tranthithanhhuyenbk.wordpress.com/2013/08/30/installing-opencv-2-2-0-on-ubuntu-12-4/

//Volvemos a Instalar HOpenCV
On Ubuntu systems, the appropriate version of OpenCV can be installed by:

 sudo apt-get install libcv-dev libhighgui-dev libcvaux-dev


---> Download HOpenCV: https://hackage.haskell.org/package/HOpenCV-0.4.0.1/HOpenCV-0.4.0.1.tar.gz
Descomprimir
cd HOpenCV

//cambiar en el archivo src/AI/CV/OpenCV/HOpenCV_wrap.c:152:12:
//     return cvHaarDetectObjects(image, cascade, storage, scale_factor, min_neighbors, flags, cvSize(width, height)); //, cvSize(0, 0));
//ESTO PASA PORQUE SE INSTALÒ OPENCV 2.0 Y ESTA FUNCIÒN EN OPENCV 2.4 CAMBIA LOS ARGUMENTOS
// VER: https://github.com/sinelaw/HOpenCV/pull/10/files

cabal configure
cabal build
cabal install




