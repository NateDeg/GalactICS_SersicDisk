mFlags=../makeflags
include $(mFlags)

all: $(GasVertObj) install


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
