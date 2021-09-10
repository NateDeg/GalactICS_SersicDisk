ccccccccc
c
c     Gas Object Definition Module
c
c     This module contains the definitions used for the
c     gas disk in GalactICS
c
ccccccccccc
c
      module GasObjDef
      use CommonConsts
      use DPFrDef
      implicit none

      Type GasObj
        real rmgas, rgas, outgas, zgas0, drtruncgas,gamma
        real gasconst
        real GasTemp
        integer nr
        real,ALLOCATABLE::rgasgrid(:),zgasgrid(:)
        real,ALLOCATABLE :: dens(:),pot(:),fr(:)
        Type(DPFrObj) DPFr  !Dens-Potential-radial Force arrays (see Objects/DPFrObj.f)
        integer nrsplg
        real eps
        real,ALLOCATABLE :: GasNorm(:),GasNormRad(:),GasNorm2(:)
        real,ALLOCATABLE :: dGasNorm(:),dGasNorm2(:)
        integer nrmass
        real drgas,rgasmax
      end Type

      contains
cccccccc
      subroutine GasIni(G,nr,dr,eps)
      implicit none
      Type(GasObj) G
      integer nr
      real dr,eps
c
      G%nr=nr
      G%gasconst = G%rmgas/(2.0*pi*G%rgas*G%rgas)
      G%eps=eps
      ALLOCATE(G%rgasgrid(0:G%nr))
      ALLOCATE(G%zgasgrid(0:G%nr))

      print*, "Gas Ini", G%nr,G%gasconst,G%eps
      call DPFrIni(G%DPFr,dr,nr)

      end subroutine
cccccccc


ccccc
      subroutine GalNormIni(G)
      implicit none
      Type(GasObj) G
      ALLOCATE(G%GasNorm(0:G%nrsplg))
      ALLOCATE(G%GasNormRad(0:G%nrsplg))
      ALLOCATE(G%GasNorm2(0:G%nrsplg))
      ALLOCATE(G%dGasNorm(0:G%nrsplg))
      ALLOCATE(G%dGasNorm2(0:G%nrsplg))

      return
      end subroutine
ccccccc

ccccc
      subroutine GalNormDeAllocate(G)
      implicit none
      Type(GasObj) G
      DEALLOCATE(G%GasNorm)
      DEALLOCATE(G%GasNormRad)
      DEALLOCATE(G%GasNorm2)
      DEALLOCATE(G%dGasNorm)
      DEALLOCATE(G%dGasNorm2)

      return
      end subroutine
ccccccc


      end module
