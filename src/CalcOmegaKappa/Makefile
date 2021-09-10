mFlags=../makeflags
include $(mFlags)

all: $(omekapObj) install


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
