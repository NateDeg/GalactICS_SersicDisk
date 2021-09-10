ccccccccc
c
c     Black Hole Object Definition Module
c
c     This module contains the definitions used for the
c     blackhole in GalactICS
c
ccccccccccc
c
      module BlackHoleObjDef
      use CommonConsts
      implicit none

      Type BlackHoleObj
        real bhmass,bhsoftening
      end Type

      end module
