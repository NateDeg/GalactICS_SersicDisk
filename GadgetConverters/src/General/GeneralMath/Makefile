mFlags=../../makeflags
include $(mFlags)

all: $(GenMath) install


StandardConsts.o:
	$(MakeFObj) StandardConsts.f

MatrixMath.o:
	$(MakeFObj) MatrixMath.f

GammaFn.o:
	$(MakeFObj) GammaFn.f

EulerRotation.o: MatrixMath.o
	$(MakeFObj) EulerRotation.f

erfcLib.o: GammaFn.o
	$(MakeFObj) erfcLib.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
