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
      module HaloGridDensMod
      use Globs
      use LegendreRoutines
      use DensFromPsiMod
      use AppDiskPotMod
      use GasDensMod
      use GenGridDensMod
      use HaloDensPsiMod

      implicit none
      contains
c
ccccccc
      subroutine GetHaloDens(lmax,ntheta)
      implicit none
      integer lmax,ntheta

cccc
      call GenGetNewDensities(lmax,ntheta,HaloPot,polarhalodens)
      return
      end subroutine
cccccc

ccccc
      real function polarhalodens(r,ctheta,l)
c returns density at r,theta, multiplied by the lth harmonic
c (must integrate this over theta to get the harmonic coeff)
      real r, ctheta,s,z
      integer l
      z=r*ctheta
      s=r*sqrt(1.0 - ctheta*ctheta)
      polarhalodens=Halodens(s,z)*plcon(l)*plgndr1(l,ctheta)
c      print*, "in polar dens 2", s,z,polardens
c     &          ,plcon(l),plgndr1(l,ctheta)
      return
      end function
ccccccc

cccccccc
      real function halodens(r,z)
      implicit none
      real r,z,psi
c
      psi=pot(r,z)
      halodens=halodenspsi(psi)
c      print*, 'Halo dens', r,z,psi,halodens

      return
      end function
ccccccccc


      end module
