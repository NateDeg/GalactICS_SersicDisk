mFlags=../../makeflags  
include $(mFlags)

all: $(GenObj) install

GalObjDef.o:
	$(MakeFObj) GalObjDef.f

PolarGridObj.o: GalObjDef.o
	$(MakeFObj) PolarGridObj.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	mv *.mod $(LocModLoc);
	mv *.o $(LocBinLoc);
