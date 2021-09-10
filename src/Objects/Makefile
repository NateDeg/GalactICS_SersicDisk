mFlags=../makeflags
include $(mFlags)

all: $(Obj) install

DPFrObj.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) DPFrObj.f

DiskObj.o: DPFrObj.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) DiskObj.f

GasObj.o: DPFrObj.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) GasObj.f

haloObj.o: DPFrObj.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) haloObj.f


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
