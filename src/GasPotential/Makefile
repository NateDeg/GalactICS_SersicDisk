mFlags=../makeflags
include $(mFlags)

all: $(GasPotObj) install


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
