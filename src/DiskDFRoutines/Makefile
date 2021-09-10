mFlags=../makeflags
include $(mFlags)

all: $(DiskDFObj) install


diskdensf.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) diskdensf.f

sig2.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) sig2.f

rcirc.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) rcirc.f

fnamidden.o: diskdensf.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) fnamidden.f

diskdfIntez.o: rcirc.o sig2.o fnamidden.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) diskdfIntez.f


CalcDiskDF.o:  sig2.o diskdensf.o diskdfIntez.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) CalcDiskDF.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
