ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module HaloDensityMod
      use NFWMod
      use TabulatedHaloMod
      use HaloObjDef
      implicit none

      contains
cccccccccc
      real function halodensity(r)
      implicit none
      real r
      real HPoint
      call HaloDensPoint(r,HPoint)
      halodensity = HPoint*eerfc(r)
c      print*, "Halo Density pointer Test",r, HPoint,halodensity,eerfc(r)

      return
      end function
ccccccccc

ccccccccc
      real function halodensprime(r)
      implicit none
      real r
      real H1Point, hPoint
      call HaloDensPoint(r,hPoint)
      call HaloDensD1Point(r,H1Point)
      halodensprime = hPoint*eerfcprime(r)+H1Point*eerfc(r)
c      halodensprime = 1
      return
      end function
cccccccc

ccccccc
      real function halodens2prime(r)
      implicit none
      real r, t1,t2,t3
      real H2Point, H1Point, hPoint

      call HaloDensPoint(r,hPoint)
      call HaloDensD1Point(r,H1Point)
      call HaloDensD2Point(r,H2Point)

      t1 = H2Point*eerfc(r)
      t2 = 2.*H1Point*eerfcprime(r)
      t3 = HPoint*eerfc2prime(r)
      halodens2prime = t1 + t2 + t3
c      halodens2prime = 1
      return
      end function
ccccccccc
      end module
