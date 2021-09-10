#Build galaxy models
BIN=../../bin



potential: dbh.dat

disk: freqdbh.dat cordbh.dat dbh.dat in.disk
	$(BIN)/gendisk < in.disk

disk2: freqdbh.dat cordbh2.dat dbh.dat in.disk2
	$(BIN)/gendisk < in.disk2 

bulge: dbh.dat in.bulge
	$(BIN)/genbulge < in.bulge

gas: dbh.dat in.gas
	$(BIN)/gengas < in.gas

halo: dbh.dat in.halo
	$(BIN)/genhalo < in.halo 

dbh.dat: in.dbh
	$(BIN)/dbh < in.dbh

freqdbh.dat: dbh.dat h.dat
	$(BIN)/getfreqs

cordbh.dat: dbh.dat freqdbh.dat in.diskdf
	$(BIN)/diskdf < in.diskdf

cordbh2.dat: dbh.dat freqdbh.dat in.diskdf2
	$(BIN)/diskdf < in.diskdf2

vr.dat: dbh.dat h.dat b.dat
	$(BIN)/vcirc 1 1 > vr.dat

clean:
	rm -f disk bulge halo galaxy

veryclean:
	rm -f *.dat disk disk2 bulge halo gasdisk galaxy *.out *.txt *.disk1 *.disk2 *.png fort.* toomre2.5 *~
