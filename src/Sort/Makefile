mFlags=../makeflags
include $(mFlags)

all: $(SortObj) install


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
