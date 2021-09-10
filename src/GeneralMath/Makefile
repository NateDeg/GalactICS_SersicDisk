mFlags=../makeflags
include $(mFlags)

all: $(MathObj) install

SimpsonsInt.o: StandardConsts.o

Factorial.o: GammaFn.o StandardConsts.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) Factorial.f

GammaFn.o: StandardConsts.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) GammaFn.f

StandardConsts.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) StandardConsts.f

clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
