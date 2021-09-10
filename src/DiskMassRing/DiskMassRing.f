ccccccccccc
c
c     MassRing
c
ccccccccc


      module MassRingMod
      use Globs
      use GenCompGlobs
      use SimpsonIntMod
      use DiskDensFMod
      implicit none

      contains


ccccc
      real function denz(z,r)
      implicit none
      real r,z
      denz=diskdensf(r,z,DUse)
      return
      end function
ccccccc


ccccc
      real function MassRing(r)
      implicit none
      real r,sum
      real Zini,ZFin
      integer nn
      nn=128
      Zini=0.
      ZFin=5.*DUse%zdisk
      call BasicSimpsonIntExtraParam(denz
     &              ,Zini,ZFin,nn
     &              ,sum,r)
      MassRing=r*sum
      return
      end function
ccccccc




      end module
