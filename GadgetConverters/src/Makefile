mFlags=makeflags 
include $(mFlags)

AllProgs=GalCombine ascii2gadget_gasIni 

all: LocalLib Programs

LocalLib: 
	cd General; make
	cd Converters; make


Programs:
	cd $(LocBinLoc); make; make install



clean:
	cd General; make clean;
	cd Converters; make clean;

	cd $(LocBinLoc); rm *.o;
	cd $(LocModLoc); rm *.mod
	cd ../Programs; rm $(AllProgs)

	

