ccccccccccccc
c
c     Disk Density-F Module
c
c       This module contains an interface for calculating the disk density
c       using the potential grid
c
cccccccccccc
c
      module DiskDensFMod
      use DiskObjDef
      use DiskDensGridMod

      implicit none
      contains
c
ccccccc
      real function diskdensf(r,z,D)
      implicit none
      real r,z,psi
      Type(DiskObj) D
      psi = pot(r,z)
      diskdensf = diskdens(r,z,psi,D)
c      print*, "diskdensf", r,z,psi,diskdensf

      return
      end function
cccccc



      end module
