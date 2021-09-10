cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module initializes the various components and
c       constants used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module iniGenGasMod
      use GenCompGlobs
      use Globs
      use GasNormMod
      use GasSurfDensMod


      implicit none

      contains

cccccccc
      subroutine IniGenGas()
      implicit none
      real gsd,f1r,f2
      real sum,r
      integer i

      print*, "Initialize Gen Gas"
      call GetGasNorm(Gas)
      call GetdGasNorm(Gas,TotPot)
      ALLOCATE(P(nPart))

      Gas%rgasmax = Gas%outgas + 4.*Gas%drtruncgas
      umin = (1. + Gas%rgasmax/Gas%rgas)*exp(-Gas%rgasmax/Gas%rgas)
      Gas%nrmass = 10000

      Gas%drgas = Gas%rgasmax/float(Gas%nrmass)

      call gassurfdens(Gas%drgas,gsd,f1r,f2,Gas)
      sum = 4.*Gas%drgas*gsd
      call gassurfdens(Gas%rgasmax,gsd,f1r,f2,Gas)
      sum = sum + Gas%rgasmax*gsd

      do i=2,Gas%nrmass-2,2
        r = Gas%drgas*float(i)
        call gassurfdens(r,gsd,f1r,f2,Gas)
        sum = sum + 2.*r*gsd
        r = r + Gas%drgas
        sum = sum + 4.*r*gsd
      enddo
      gasdiskmass = 2.*pi*sum*Gas%drgas/3.
      gasdiskparticlemass = gasdiskmass/float(nPart)
      time=0.


      return
      end subroutine
ccccccc


      end module
