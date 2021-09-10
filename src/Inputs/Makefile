mFlags=../makeflags
include $(mFlags)

all: $(InputObj) install


genHaloInputs.o:  ReadHarmFile.o ReadMassRad.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) genHaloInputs.f

genBulgeInputs.o:  ReadHarmFile.o ReadMassRad.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) genBulgeInputs.f

genDiskInputs.o:  ReadHarmFile.o ReadMassRad.o ReadDiskDF.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) genDiskInputs.f

getFreqsInputs.o:  ReadHarmFile.o 
	$(F77) $(FLAGS) -c -I$(LocModLoc) getFreqsInputs.f

ReadDiskDF.o:  
	$(F77) $(FLAGS) -c -I$(LocModLoc) ReadDiskDF.f

diskdfInputs.o:  ReadHarmFile.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) diskdfInputs.f

ReadHarmFile.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) ReadHarmFile.f

ReadMassRad.o: 
	$(F77) $(FLAGS) -c -I$(LocModLoc) ReadMassRad.f


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
