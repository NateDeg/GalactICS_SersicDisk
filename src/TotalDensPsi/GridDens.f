ccccccccccccc
c
c     Grid Density Module
c
c       This module contains routines for calculating the density using
c       the potential arrays and the polardens routines
c
c     NB that dens only gives the density without an approximate sech**2
c     component --- that part of the potential is not represented by the
c     harmonics.  The function dens(r,z) returns the density -
c     high-frequency cpt The full density is returned by
c
c
cccccccccccc
c
      module GridDensMod
      use Globs
      use LegendreRoutines
      use DensFromPsiMod
      use AppDiskPotMod
      use GasDensMod
      use GenGridDensMod

      implicit none
      contains
c
ccccccc
      subroutine GetNewDensities(lmax,ntheta)
      implicit none
      integer lmax,ntheta
cccc

      call GenGetNewDensities(lmax,ntheta,TotPot,polardens)
c      print*, "New Densities routine"

      return
      end subroutine
cccccc

ccccc
      real function polardens(r,ctheta,l)
c returns density at r,theta, multiplied by the lth harmonic
c (must integrate this over theta to get the harmonic coeff)
      real r, ctheta,s,z
      integer l
      z=r*ctheta
      s=r*sqrt(1.0 - ctheta*ctheta)
      polardens=dens(s,z)*plcon(l)*plgndr1(l,ctheta)
c      if(polardens .lt. 0.) then
c        print*, "in polar dens 2", s,z,polardens
c     &          ,plcon(l),plgndr1(l,ctheta)
c      endif
      return
      end function
ccccccc

cccccc
      real function dens(r,z)
      implicit none
      real r,z
      real addens,gDens
      real add1,add2,add3

      gdens=totdens_FrPsi(r,z)
      dens=gdens
      addens=0.
      if(DiskFlag1) then
        add1 = appdiskdens(r,z,D1)
      else
        add1 = 0.
      endif
      if(DiskFlag2) then
        add2 = addens + appdiskdens(r,z,D2)
      else
        add2=0.
      endif
      if(GasFlag) then
        add3 = addens + appgasdens(r,z,Gas)
      else
        add3=0.
      endif
      addens=add1+add2+add3
      dens = dens - addens

      if(dens .lt. 0.) then
cc        print*, r,z,gdens-addens,gdens,addens,add1,add2,add3
        dens=0.
      endif

      return
      end function
ccccccc

      end module
