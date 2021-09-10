mFlags=../makeflags
include $(mFlags)

all: $(TDensPsiObj) install

GridDens.o: GenGridDens.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) GridDens.f

DensityFromPsi.o: diskdens.o gasdenspsi.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) DensityFromPsi.f

BulgeGridDens.o:GenGridDens.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) BulgeGridDens.f

HaloGridDens.o:GenGridDens.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) HaloGridDens.f

GenGridDens.o: DensityFromPsi.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) GenGridDens.f


diskdens.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) diskdens.f

gasdenspsi.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) gasdenspsi.f



clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
