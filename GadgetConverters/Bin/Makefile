mFlags=../src/makeflags 
include $(mFlags)

VPATH :=$(BinLoc)

all: GalCombine ascii2gadget_gasIni 

GalCombine: GalCombine.o 
	$(F77) $(FLAGS) -o GalCombine GalCombine.o  $(GeneralObjects) $(ConvertObj) $(FitsLibLoc)

ascii2gadget_gasIni: ascii2gadget_gasIni.o
	$(CC) $(CFLAGS) -o ascii2gadget_gasIni ascii2gadget_gasIni.o


install:
	mv GalCombine ../Programs
	mv ascii2gadget_gasIni ../Programs



clean:
	rm -f $(all);
	rm -f *.o;


