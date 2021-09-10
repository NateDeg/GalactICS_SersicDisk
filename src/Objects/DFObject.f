ccccccccc
c
c     Distribution function Object Definition Module
c
c     This module contains the definitions used for the
c     DFs in GalactICS
c
ccccccccccc
c
      module DFObjDef
      use CommonConsts
      use DPFrDef
      implicit none

      Type DFObj
        integer npsi,nint
        real psi0,psic, psid,psi0mpsid,psi00
        real,ALLOCATABLE :: tableE(:),dfsersic(:),dfnfw(:)
        real,ALLOCATABLE :: denspsibulge(:), denspsihalo(:)
      end Type

      contains
cccccccc
      subroutine DFIni(D,npsi,nint)
      implicit none
      Type(DFObj) D
      integer npsi,nint
c
      D%npsi=npsi
      D%nint=nint
      ALLOCATE(D%tableE(npsi))
      ALLOCATE(D%dfsersic(npsi))
      ALLOCATE(D%denspsibulge(npsi))
      ALLOCATE(D%dfnfw(npsi))
      ALLOCATE(D%denspsihalo(npsi))
      return
      end subroutine
cccccccc

cccccc
      real function DFcoef(j)
      implicit none
      integer j
      if(j.eq.1) DFcoef=17./48.
      if(j.eq.2) DFcoef=59./48.
      if(j.eq.3) DFcoef=43./48.
      if(j.eq.4) DFcoef=49./48.
      return
      end function
cccccc
      end module
