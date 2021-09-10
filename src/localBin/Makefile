mFlags=../makeflags 
include $(mFlags)

VPATH :=$(BinLoc)

all: dbh genhalo genbulge gendisk getfreqs diskdf gengas


dbh: dbh.o 
	$(F77) $(FLAGS) -o dbh dbh.o  $(LocalObj)

genhalo: genhalo.o 
	$(F77) $(FLAGS) -o genhalo genhalo.o  $(LocalObj)

genbulge: genbulge.o 
	$(F77) $(FLAGS) -o genbulge genbulge.o  $(LocalObj)

gendisk: gendisk.o 
	$(F77) $(FLAGS) -o gendisk gendisk.o  $(LocalObj)

getfreqs: getfreqs.o 
	$(F77) $(FLAGS) -o getfreqs getfreqs.o  $(LocalObj)

diskdf: diskdf.o 
	$(F77) $(FLAGS) -o diskdf diskdf.o  $(LocalObj)

gengas: gengas.o 
	$(F77) $(FLAGS) -o gengas gengas.o  $(LocalObj)




install:
	mv dbh ../;
	mv genhalo ../;
	mv genbulge ../;
	mv gendisk ../;
	mv getfreqs ../;
	mv diskdf ../;
	mv gengas ../;


clean:
	rm -f $(all);
	rm -f *.o;


