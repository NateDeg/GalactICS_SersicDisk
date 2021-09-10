Installing GalactICS

Installing GalactICS is a relatively straightforward matter.  
1) In the GalactICS/src folder open the makeflags file. Replace LocalPath variable with
	the path of the src directory.
2) Set F77 to your fortran compiler (I use gfortran).
3) cd into the src directory.  Type ‘make clean’ to make sure 
	all old files are removed.
4) Type ‘make’
5) Type ‘make install’