.PHONY : clean

CVINPUT=-I /usr/local/include/
CVFLAGS=-L/usr/local/lib -lopencv_core
CVCOMPILE=`pkg-config opencv --cflags --libs`
DIR= HOpenCV/CV
FILES=load canny Test video

all: $(FILES)

clean :
	rm -f *.o
	rm -f *.gch
	rm -f *.hi

canny: 
	ghc --make cannyVideo.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE) -o canny

load: 
	ghc --make load.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE) -o load

Test: 
	ghc --make Test.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE) -o Test

video: 
	ghc --make VideoWriter.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE) -o video















