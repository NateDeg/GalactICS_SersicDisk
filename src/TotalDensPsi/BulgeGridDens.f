ccccccccccccc
c
c     Bulge Grid Density Module
c
c       This module contains routines for calculating the density using
c       the potential arrays and the polardens routines
c
cccccccccccc
c
      module BulgeGridDensMod
      use Globs
      use LegendreRoutines
      use DensFromPsiMod
      use AppDiskPotMod
      use GasDensMod
      use GenGridDensMod
      use BulgeDensPsiMod

      implicit none
      contains
c
ccccccc
      subroutine GetBulgeDens(lmax,ntheta)
      implicit none
      integer lmax,ntheta

cccc
      call GenGetNewDensities(lmax,ntheta,BulgePot,polarbulgedens)
      return
      end subroutine
cccccc

ccccc
      real function polarbulgedens(r,ctheta,l)
c returns density at r,theta, multiplied by the lth harmonic
c (must integrate this over theta to get the harmonic coeff)
      real r, ctheta,s,z
      integer l
      z=r*ctheta
      s=r*sqrt(1.0 - ctheta*ctheta)
      polarbulgedens=Bulgedens(s,z)*plcon(l)*plgndr1(l,ctheta)
c      print*, "in polar dens 2", s,z,polardens
c     &          ,plcon(l),plgndr1(l,ctheta)
      return
      end function
ccccccc

cccccccc
      real function Bulgedens(r,z)
      implicit none
      real r,z,psi
c
      psi=pot(r,z)
      Bulgedens=Bulgedenspsi(psi)

      return
      end function
ccccccccc


      end module
