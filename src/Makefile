mFlags=makeflags 
include $(mFlags)
	 

VPATH :=$(BinLoc)

AllProgs= dbh genhalo genbulge getfreqs gendisk diskdf gengas

all: LocalLib Programs

LocalLib: 
	cd GeneralMath; make;
	cd Sort; make;
	cd Objects; make;
	cd Globals; make;
	cd Halo; make;
	cd Disk; make;
	cd GasSurfDens; make;
	cd Bulge; make;
	cd BlackHole; make;
	cd TotalDensity; make;
	cd GasVerticalProfile; make;
	cd GasDensity; make;
	cd GasPotential; make;
	cd TotalPotentialEstimate; make;
	cd CalcForce; make;
	cd CalcPotFromGrid; make;
	cd GasVertGridfromPot; make;
	cd CalcPsi; make;
	cd GenDistFn; make;
	cd GenDensPsi; make;
	cd TotalDensPsi; make;
	cd CalcTotalPotential; make;
	cd CalcOmegaKappa; make;
	cd DiskDFRoutines; make;
	cd DiskMassRing; make;
	cd Initialization; make;
	cd Inputs; make;
	cd CalcFreqs; make;
	cd MakeComponent; make;
	cd Outputs; make;
	cd COMAdjust; make;
	cd Main; make;


Programs:
	cd $(LocBinLoc); make; make install

install:
	cp -f $(AllProgs) ../bin



%.o: %.f
	$(F77) -c $(Flags) $< -I$(ModLoc)


clean:
	cd GeneralMath; make clean;
	cd Sort; make clean;
	cd Objects; make clean;
	cd Halo; make clean;
	cd Disk; make clean;
	cd GasSurfDens; make clean;
	cd Bulge; make clean;
	cd BlackHole; make clean;
	cd Globals; make clean;
	cd Inputs; make clean;
	cd TotalDensity; make clean;
	cd GasVerticalProfile; make clean;
	cd GasDensity; make clean;
	cd GasPotential; make clean;
	cd Initialization; make clean;
	cd CalcOmegaKappa; make clean;
	cd TotalPotentialEstimate; make clean;
	cd CalcForce; make clean;
	cd CalcPotFromGrid; make clean;
	cd GasVertGridfromPot; make clean;
	cd CalcPsi; make clean;
	cd GenDistFn; make clean;
	cd GenDensPsi; make clean;
	cd TotalDensPsi; make clean;
	cd Outputs; make clean;
	cd CalcTotalPotential; make clean;
	cd CalcFreqs; make clean;
	cd DiskDFRoutines; make clean;
	cd DiskMassRing; make clean;
	cd MakeComponent; make clean;
	cd COMAdjust; make clean;
	cd Main; make clean;
	cd $(LocBinLoc); rm *.o;
	cd $(LocModLoc); rm *.mod
	rm $(AllProgs)
	cd ../bin; rm $(AllProgs)

	

