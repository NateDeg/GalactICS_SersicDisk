ccccccccc
c
c     Disk Object Definition Module
c
c     This module contains the definitions used for the
c     disk in GalactICS
c
ccccccccccc
c
      module PotObjDef
      use CommonConsts
      implicit none

      Type PotObj
        integer nr,lmax
        real,ALLOCATABLE :: pot(:,:),Fr(:,:),Dens(:,:),Fr2(:,:)
        real potcor,potcor1
        real,ALLOCATABLE :: potmaj(:),potmin(:),potup(:)
        real,ALLOCATABLE :: vcmaj(:),vertfreq(:),psi2(:)
      end Type

      contains
cccccccc
      subroutine PotIni(P,nr,lmax)
      implicit none
      Type(PotObj) P
      integer nr,lmax
c

c      print*, "Potential Ini"
      P%nr=nr
      P%lmax=lmax

      ALLOCATE(P%pot(0:P%lmax,0:P%nr))
      ALLOCATE(P%Fr(0:P%lmax,0:P%nr))
      ALLOCATE(P%Fr2(0:P%lmax,0:P%nr))
      ALLOCATE(P%Dens(0:P%lmax,0:P%nr))
      ALLOCATE(P%potmaj(0:P%nr))
      ALLOCATE(P%potmin(0:P%nr))
      ALLOCATE(P%potup(0:P%nr))
      ALLOCATE(P%vcmaj(0:P%nr))
      ALLOCATE(P%vertfreq(0:P%nr))
      ALLOCATE(P%Psi2(0:P%nr))


      end subroutine
cccccccc

      end module
