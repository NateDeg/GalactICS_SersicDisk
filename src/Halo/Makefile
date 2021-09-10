mFlags=../makeflags
include $(mFlags)

all: $(HaloObj) install

halopotentialestimate.o: halodensity.o

halodensity.o: nfwprofiles.o TabulatedHaloProfile.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) halodensity.f

nfwprofiles.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) nfwprofiles.f

TabulatedHaloProfile.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) TabulatedHaloProfile.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
