mFlags=../makeflags
include $(mFlags)

all: $(GasSurfDensObj) install


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
