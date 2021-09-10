mFlags=../makeflags
include $(mFlags)

all: $(OutputObj) install

GenDensPotForceOut.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) GenDensPotForceOut.f

DBHOutputs.o: GenDensPotForceOut.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) DBHOutputs.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
