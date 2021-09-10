mFlags=../makeflags
include $(mFlags)

all: $(MakeCompObj) install

MakeSphericalComponent.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) MakeSphericalComponent.f

MakeHalo.o: MakeSphericalComponent.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) MakeHalo.f

MakeBulge.o: MakeSphericalComponent.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) MakeBulge.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
