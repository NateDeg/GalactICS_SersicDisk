cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains declarations for all globals
c     used in GalactICS
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module Globs
      use HaloObjDef
      use DiskObjDef
      use GasObjDef
      use BulgeObjDef
      use BlackHoleObjDef
      use PotObjDef
      use DFObjDef

      implicit none


      Logical BulgeFlag,DiskFlag1,DiskFlag2,GasFlag,HaloFlag,BHFlag
      Type(HaloObj) Halo
      Type(DiskObj) D1,D2,DUse
      Type(GasObj) Gas
      Type(BulgeObj) Bulge
      Type(BlackHoleObj) BH

      Type(PotObj) TotPot,HaloPot,BulgePot
      Type(DFObj) DF

      real dr
      integer nr, lmaxx

      integer npsi,nint,niter

      real,parameter :: eps=0.0001

      real,ALLOCATABLE :: plcon(:)            !Legendre Constants
      logical MaxIterFlag

      end module

