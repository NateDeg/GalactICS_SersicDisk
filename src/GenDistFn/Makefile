mFlags=../makeflags
include $(mFlags)

all: $(DFObj) install



gendfbulge.o: generaldfRoutines.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) gendfbulge.f

gendfHalo.o: generaldfRoutines.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) gendfHalo.f

generaldfRoutines.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) generaldfRoutines.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
