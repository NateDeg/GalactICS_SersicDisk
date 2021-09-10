# GalactICS_SersicDisk
 A code for generating multi-component equilibrium N-Body models of galaxies.
 
 Version 1.0
 
 ----
 Authors: N. Deg, L. Widrow, V. Debattista
 References: Deg et al. (in prep), Deg et al. 2018, Wang et al. 2010, Widrow et al. 2008
 ----
 Basic Description
 
 GalactICS (Galaxy Initial ConditionS) is a code that generates N-Body realizations of equilibrium galaxy models.  The models may consist of a dark matter halo, up to 2 stellar disks, a gas disk, and a stellar bulge.  The halo is a double power-law, the bulge is described by a Sérsic profile, and the gas disk is generated using the methodology described in Deg et al. (2018) and Wang et al. (2010).  The stellar disks are now generated with a Sérsic surface density rather than just an exponential disk (note that when the Sérsic index, n, equal 1, the profile is exponential).
 
 More detailed instructions on the installation and usage of the code can be found in the /READMES.
 
 ---
 Dependencis
 gfortran
 
 ---
 Quick installation
 
 In terminal:
 1) cd src
 2) make clean
 3) make
 4) make install
 
 -----
 Quick usage
 
 In terminal:
 1) cd into the target 'model' directory (see the same model in the models folder)
 2) make potential
 3) make each component individually (i.e. make halo, make disk, make disk2, make bulge, make gas)
 4) Combine the resulting ascii files into an appropriate format for your N-body software.
