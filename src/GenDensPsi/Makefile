mFlags=../makeflags
include $(mFlags)

all: $(GenDPObj) install

gendenspsibulge.o: gendenspsi.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) gendenspsibulge.f

gendenspsihalo.o: gendenspsi.o
	$(F77) $(FLAGS) -c -I$(LocModLoc) gendenspsihalo.f

gendenspsi.o:
	$(F77) $(FLAGS) -c -I$(LocModLoc) gendenspsi.f


clean:
	rm -f *.mod;
	rm -f *.o;

install:
	cp *.mod $(LocModLoc);
	cp *.o $(LocBinLoc);
