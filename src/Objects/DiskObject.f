ccccccccc
c
c     Disk Object Definition Module
c
c     This module contains the definitions used for the
c     disk in GalactICS
c
ccccccccccc
c
      module DiskObjDef
      use CommonConsts
      use DPFrDef
      use GammaFnRoutines
      implicit none

      Type DiskObj
        real rmdisk, rdisk, outdisk, zdisk, drtrunc,
     &          rhole, rcore,diskconst,ndisk
        Type(DPFrObj) DPFr  !Dens-Potential-radial Force arrays (see Objects/DPFrObj.f)
        logical DiskUseFlag
        real eps
        real,ALLOCATABLE :: surden(:)
c        real sigr0,disksr,sigrcrit,rfid,sigr,sigden,sig_n
        real sigrcrit,rfid,sigr,sigden
        real sigr01,disksr1,sigr02,disksr2
        real qtoomre
        integer nrspl,nrdisk
        real drspl
        real,ALLOCATABLE :: rr(:),fdrat(:),fszrat(:),fzrat(:)
        real,ALLOCATABLE :: drat(:),drat2(:),szrat2(:)
        real,ALLOCATABLE :: dz2rat(:)
        real,ALLOCATABLE :: d0rat(:),d1rat(:),d2rat(:)
     &                      ,d3rat(:),d4rat(:)

        real rtrunc,rd,zd,diskmass
      end Type

      contains
cccccccc
      subroutine DiskIni(D,dr,nr)
      implicit none
      Type(DiskObj) D
      integer nr
      real dr
c
      D%DiskUseFlag=.True.

      D%eps=0.0001
      D%nrdisk= int((D%outdisk + 2.0*D%drtrunc)/dr) + 10
c      D%diskconst = D%rmdisk/(2.0*pi*D%rdisk*D%rdisk)
      D%diskconst = D%rmdisk/(2.0*pi*D%rdisk*D%rdisk*D%ndisk
     &                  *exp(gammln(2.*D%ndisk)))

      print*, "Disk Ini", D%rmdisk,D%rdisk,D%outdisk,D%zdisk
     &              ,D%drtrunc, D%rhole,D%rcore
     &              ,D%ndisk,D%diskconst
      print*, "Gamma Test", exp(gammln(2.*D%ndisk))


      ALLOCATE(D%surden(0:nr))
      call DPFrIni(D%DPFr,dr,nr)

      end subroutine
cccccccc

ccccc
      subroutine DiskDFALLOCATE(D)
      implicit none
      Type(DiskObj) D
      ALLOCATE(D%rr(0:D%nrspl))
      ALLOCATE(D%drat(0:D%nrspl))
      ALLOCATE(D%fdrat(0:D%nrspl))
      ALLOCATE(D%fszrat(0:D%nrspl))
      ALLOCATE(D%fzrat(0:D%nrspl))
      ALLOCATE(D%drat2(0:D%nrspl))
      ALLOCATE(D%dz2rat(0:D%nrspl))
      ALLOCATE(D%szrat2(0:D%nrspl))

      ALLOCATE(D%d0rat(0:D%nrspl))
      ALLOCATE(D%d1rat(0:D%nrspl))
      ALLOCATE(D%d2rat(0:D%nrspl))
      ALLOCATE(D%d3rat(0:D%nrspl))
      ALLOCATE(D%d4rat(0:D%nrspl))
      return
      end subroutine


ccccccc
      end module
