cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module initializes the various components and
c       constants used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module iniDBHMod
      use Globs
      use SersicProfilesMod

      implicit none

      contains

      subroutine IniDBH()
      implicit none

c       Galaxy Component Initialize
      call GalIni()

      return
      end subroutine


      subroutine GalIni()
      implicit none
      integer i
c
      print*, "Initializing Galaxy Components"
     &              ,DiskFlag1,DiskFlag2
     &              ,GasFlag,BulgeFlag,HaloFlag
c       Initialize the halo if necessary
      if(HaloFlag) then
        call HaloIni(Halo,dr,nr)
        call HaloFuncIni()
      endif

c       Initialize the first disk
      if(DiskFlag1) call DiskIni(D1,dr,nr)
c       Initialize the second disk
      if(DiskFlag2) call DiskIni(D2,dr,nr)
c       Initialize the gas disk
      if(GasFlag) call GasIni(Gas,nr,dr,eps)
c       Initialize the bulge
      if(BulgeFlag) call BulgeIni(Bulge,BulgeFlag)
c       Nothing needs to be initialized for the Black Hole

c       Initialize the Potential Arrays
      call  PotIni(TotPot,nr,lmaxx)
      call  PotIni(HaloPot,nr,lmaxx)
      call  PotIni(BulgePot,nr,lmaxx)
c       Initialize the legendre constants
      ALLOCATE(plcon(0:lmaxx))
      do i=0, lmaxx
        plcon(i) = sqrt((2*i+1)/(4.0*pi))
c        print*, "Initial", plcon(i)
      enddo

c       Initialize the Distribution function object
      call DFIni(DF,npsi,nint)        !see Objects/DFObject.f

      return
      end subroutine
ccccccc


cccccccc
      subroutine HaloFuncIni()
      use NFWMod
      use TabulatedHaloMod
      use AlpahBetaGammaHaloMod
      implicit none
      real RTest,DTest
c      real,external nfwdens
c
      print*, "Setting the halo density function pointer"

      if(Halo%NumericalTableSwitch .eq. 1) then
        HaloDensPoint=> TabulatedDens
        HaloDensD1Point=> TabulatedDens1Prime
        HaloDensD2Point=> TabulatedDens2Prime
      elseif(Halo%NumericalTableSwitch .eq. 0) then
        HaloDensPoint=> nfwdens
        HaloDensD1Point=> nfwdensprime
        HaloDensD2Point=> nfwdens2prime
      elseif(Halo%NumericalTableSwitch .eq. 2) then
        HaloDensPoint=> ABGDens
        HaloDensD1Point=> ABGDens1Prime
        HaloDensD2Point=> ABGDens2Prime
      endif

c      RTest=1.
c      call HaloDensPoint(RTest,DTest)
c      print*, "HaloPoint Test 1", DTest,Halo%NumericalTableSwitch


      return
      end subroutine
cccccccccc

      end module
