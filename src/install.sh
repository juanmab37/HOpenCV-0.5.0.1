
sudo apt install ghc
sudo apt install cabal-install
cabal update
cabal install base #Aca esta Foreing
cabal install hsc2hs
cabal install vector-space
cabal install allocated-processor
#Control.Monad.Error
sudo apt-get install haskell-platform

#Install OpenCV 3
wget https://github.com/Itseez/opencv/archive/3.1.0.zip
unzip 3.1.0.zip

#dependencias opencv
sudo apt-get install cmake git libgtk2.0-dev pkg-config libavcodec-dev libavformat-dev libswscale-dev
sudo apt-get install libtbb2 libtbb-dev libjpeg-dev libpng-dev libtiff-dev libjasper-dev libdc1394-22-dev

cd ~/opencv
mkdir release
cd release
sudo apt-get install cmake
#cmake -DWITH_IPP=ON -DINSTALL_CREATE_DISTRIB=ON . 
cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local ..
make
sudo make install



