mFlags=../makeflags
include $(mFlags)

all: $(DiskObj) install

diskpotentialestimate.o: diskdensity.o

appdiskpot.o: diskdensity.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) appdiskpot.f

diskdensity.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) diskdensity.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
