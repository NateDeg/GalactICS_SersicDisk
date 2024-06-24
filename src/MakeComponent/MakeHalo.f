ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module MakeHaloMod
      use Globs
      use GenCompGlobs
      use HaloGridDensMod
      use HaloDFMod
      use MakeSphComponentMod

      implicit none

      contains

ccccccc
      subroutine MakeHalo()
      implicit none
      integer i,count,j
      Type(Particle) PTest
      logical pCheck

      count=0
      j=0
      print*, "Calculating halo positions and velocities"
      do i=1, nPart
100     pCheck=.False.
        count=count+1
c        print*, i,count
        call GetTestPos(PTest,pCheck,halodens)
        if(pCheck) goto 100
        call GetTestVel(PTest,pCheck,fcut_halo,dfhalo)
        if(pCheck) goto 100
        call EnergyCalc(PTest)
        call RandomFlip(PTest)

        j=j+1
        P(j)=PTest
        P(j)%Mass=pMass
        if( mod(j,1000) .eq. 0) write(*,101,advance='no')"."
        if(j .eq. nPart) goto 200
c           Section to force vertical symmetry
c        j=j+1
c        PTest%Pos(3)=-PTest%Pos(3)
c        PTest%Vel(3)=-PTest%Vel(3)
c        P(j)=PTest
c        P(j)%Mass=pMass
c        call EnergyCalc(PTest)
c        if( mod(j,1000) .eq. 0) write(*,101,advance='no')"."
c        if(j .eq. nPart) goto 200
      enddo
200   continue
      print*, " "
      print*, "2*kinetic/potential",2.*kinetic/potential
      print*, " "

101   format(A)

      return
      end subroutine
ccccccccc

cccccc
      subroutine RandomFlip(P)
      use RandomMod
      implicit none
      Type(Particle) P
      real FlipTest

      FlipTest=ran3(idum)
      if(FlipTest .ge. 0.5) then
        P%Pos(3)=-P%Pos(3)
        P%Vel(3)=-P%Vel(3)
      endif
      return
      end subroutine
cccccc

      end module
