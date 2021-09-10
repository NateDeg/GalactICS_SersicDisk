mFlags=../../makeflags
include $(mFlags)

all: $(CoordObj) install

CartGalConvert.o: RecenterCart.o
	$(MakeFObj) CartGalConvert.f

RecenterCart.o:
	$(MakeFObj) RecenterCart.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
