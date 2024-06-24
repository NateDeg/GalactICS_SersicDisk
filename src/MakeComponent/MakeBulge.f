ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module MakeBulgeMod
      use Globs
      use GenCompGlobs
      use BulgeGridDensMod
      use BulgeDFMod
      use MakeSphComponentMod

      implicit none

      contains

ccccccc
      subroutine MakeBulge()
      implicit none
      integer i,count
      Type(Particle) PTest
      logical pCheck

      count=0
      print*, "Calculating halo positions and velocities"
      do i=1, nPart
100     pCheck=.False.
        count=count+1
c        print*, i,count
        call GetTestPos(PTest,pCheck,bulgedens)
        if(pCheck) goto 100
        call GetTestVel(PTest,pCheck,fcut_bulge,dfbulge)
        if(pCheck) goto 100
        call EnergyCalc(PTest)
        P(i)=PTest
        P(i)%Mass=pMass
        if( mod(i,1000) .eq. 0) write(*,101,advance='no')"."
      enddo
      print*, " "
      print*, "2*kinetic/potential",2.*kinetic/potential
      print*, " "

101   format(A)

      return
      end subroutine
ccccccccc


      end module
