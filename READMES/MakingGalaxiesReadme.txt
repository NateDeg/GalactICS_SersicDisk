Making a Galaxy with GalactICS

Building a galaxy with GalactICS takes a number of steps.

Firstly, a quick note on GalactICS.  GalactICS is designed to generate
near-equilibrium galaxies by solving for the distribution function.

The galaxy may consist of up to 5 components —- halo, bulge, 2 stellar disks, 
and a gas disk.

—————
There are a number of steps in making a galaxy.

1) cd into the models directory
2) cd into the specific model directory you wish to build (try HSB_A)
3) type ‘make potential’ to build the potential-density pair for the model
4) type ‘make gas’ to build the gas disk
5) type ‘make disk’ to build disk1
6) type ‘make disk2’ to build the second disk
7) type ‘make halo’ to build the halo
8) type ‘make bulge’ to build the bulge

————
Main output formats

GalactICS produces a number of output files.  The ones that contain
the N-body particles are ‘gasdisk’, ‘bulge’, ‘disk’, ‘disk2’, and ‘halo’.
The first line is:
number of particles		dummy variable
The rest are
Mass	X	Y	Z	VX	VY	VZ

For the gas disk, VZ is actually the gas particle entropy’s which are later 
placed into the correct column for Gadget.

The units are 2.325e9 M_sol, kpc, and 100 km/s

For models without a bulge, it is necessary to create an empty file named ‘b.dat’

——
Input files

***
in.dbh

This file contains the parameters used to make the density-potential pair.  It
is perhaps the most important of the various in. files.

File Format

y						#Use Halo
  100. 4.25   20.1  20.  1. 3.			# R_trunc, sigma_h, R_h, d_rtrunc, alpha, beta
y						#Use Disk 1
  15.  2.7  25.  0.36  3. 0.	  0.		# M_disk1, R_d1,R_dtrunc1,z_d1,z_trunc1, dummy 1, dummy 2
n						#Use Disk2 (if y, next line is same format as Disk1
y						#Use gas disk
 1.5 4.0 25. .0083 3.  -1.6667			#M_gas, R_gas, R_trunc, G_Entropy, dR_gastrunc, Gamma (keep at -5/3)
y						#Use Bulge
  2. -1.  3.5  1.0 40. 10.			#Sersic Index, Calc p switch, sigma_b, R_b, dummy, dummy
n						#Use SMBH (dummy switch at the moment
0.01 50000					#dr, nr
10						#lmax


***
in.bulge

This file contains the parameters for making the bulge after making the potential.

File Format
 0.5						#Streaming Faction
 2000000					#number of bulge particles
 -8						#random number seed
 1						#center of mass switch

***
in.disk (in.disk2)

This file contains some of the parameters used for making the disk.

File Format
1					#Disk 1 or 2 switch
2000000					#number of particles
-1					#random integer seed
1					#1=yes we want to center 0=no we don't
dbh.dat					#multipole expansion data file (from make potential)

***
in.diskdf

This file contains parameters that determine the velocity distribution of the disk distribution function.

File Format
 1					#Disk 1 or 2 switch
 1.0 2.2				#sigma_r, R_sigma (usually R_sigma=R_d)
 50					#number of radial spline points
 10					#number of iterations
 psfile					#dummy line

***
in.halo

This file contains parameters used for making the halo.

File Format
0.5					#Streaming Faction
10000000				#number of halo particles
-1					#random number seed
1					#center of mass switch
dbh.dat					#multipole expansion data file (from make potential)

***
in.gas

This file contains parameters used for making the gas disk.

File Format
500000					#number of particles
-1					#random integer seed
1					#1=yes we want to center 0=no we don't
dbh.dat					#multipole expansion data file
100					#number of points for normalization spline
1					#Adiabatic or Isothermal EOS for Gadget (1 or 2)

***
in.gendenspsi

This file contains parameters for the calculation of the potential

File Format
1000 20					#n_psi, n_int

	
