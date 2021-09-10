ccccccccc
c
c     Bulge Object Definition Module
c
c     This module contains the definitions used for the
c     bulge in GalactICS
c
ccccccccccc
c
      module BulgeObjDef
      use CommonConsts
      implicit none

      Type BulgeObj
        real nnn, ppp, v0bulge, abulge
        real Re, Rho0, butt
      end Type

      end module
