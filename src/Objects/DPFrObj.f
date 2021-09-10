ccccccccc
c
c     Dens-Pot-Radial Force Definition Module
c
c     This module contains the definitions used for the
c     density-potential-radial force arrays used in gas,disk,and halo
c     objects
c
ccccccccccc
c
      module DPFrDef
      use CommonConsts
      implicit none

      Type DPFrObj
        integer nr
        real dr
        real,ALLOCATABLE :: dens(:),pot(:),fr(:)
      end Type

      contains
cccccccc
      subroutine DPFrIni(Grid,dr,nr)
      implicit none
      Type(DPFrObj) Grid
      integer nr
      real dr

c
      Grid%nr=nr
      Grid%dr=dr

      ALLOCATE(Grid%dens(0:Grid%nr))
      ALLOCATE(Grid%pot(0:Grid%nr))
      ALLOCATE(Grid%fr(0:Grid%nr))


      end subroutine
cccccccc

      end module
