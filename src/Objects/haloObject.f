ccccccccc
c
c     Halo Object Definition Module
c
c     This module contains the definitions used for the
c     halo in GalactICS
c
ccccccccccc
c
      module HaloObjDef
      use CommonConsts
      use DPFrDef
      implicit none


      PROCEDURE(RadialFuncInterface),POINTER :: HaloDensPoint =>null()
      PROCEDURE(RadialFuncInterface),POINTER :: HaloDensD1Point
      PROCEDURE(RadialFuncInterface),POINTER :: HaloDensD2Point

      ABSTRACT INTERFACE
        subroutine RadialFuncInterface(R,Result)
            IMPLICIT NONE
            real,intent(IN) :: R
            real,intent(out) :: Result

        END subroutine RadialFuncInterface
      END INTERFACE



      Type HaloObj
        real chalo,v0,a,drtrunchalo,cusp,haloconst
        real outerslope
        real eps
        Type(DPFrObj) DPFr  !Dens-Potential-radial Force arrays (see Objects/DPFrObj.f)
        integer NUmericalTableSwitch
        character(100) HaloProfileFile
        integer nRProfile
        real,ALLOCATABLE :: RProf(:),DProf(:),D1Prof(:),D2Prof(:)

        real rhoS, Rs, alpha,beta,gamma
      end Type

      contains
cccccccc
      subroutine HaloIni(H,dr,nr)
      implicit none
      Type(HaloObj) H
      integer nr
      real dr

c
      if(H%NumericalTableSwitch .eq.1) then
        call ReadNumericalHaloProfile(H)
        H%eps=0.0001
        call DPFrIni(H%DPFr,dr,nr)
      elseif(H%NumericalTableSwitch .eq. 0) then
        H%eps=0.0001
c      H%haloconst=(2.**(1.-H%cusp))*H%v0*H%v0/4./pi/H%a/H%a
        H%haloconst = (2.**(1.-H%cusp))
     &          *H%v0*H%v0/4./pi/H%a/H%a
        call DPFrIni(H%DPFr,dr,nr)
        print*, "HaloConts",H%chalo, H%v0, H%a, H%drtrunchalo
     &          ,H%cusp,H%haloconst
      elseif(H%NumericalTableSwitch .eq. 2) then
        call DPFrIni(H%DPFr,dr,nr)
      endif

      return
      end subroutine
cccccccc

ccccccc
      subroutine ReadNumericalHaloProfile(H)
      implicit none
      Type(HaloObj) H
      integer i

      open(10,file=H%HaloProfileFile,status='old')
      read(10,*) H%nRProfile
      print*, "Size of Halo Profile", H%nRProfile
      ALLOCATE(H%RProf(H%nRProfile))
      ALLOCATE(H%DProf(H%nRProfile))
      ALLOCATE(H%D1Prof(H%nRProfile))
      ALLOCATE(H%D2Prof(H%nRProfile))
      do i=1, H%nRProfile
        read(10,*) H%RProf(i),H%DProf(i),H%D1Prof(i),H%D2Prof(i)
c        H%RProf(i)=log(H%RProf(i))
        if(abs(H%DProf(i)) .le. 1.e-20) H%DProf(i)=1.e-20
c        H%DProf(i)=log(H%DProf(i)/mscale)
c        H%D1Prof(i)=log(-H%D1Prof(i)/mscale)
c        H%D2Prof(i)=log(H%D2Prof(i)/mscale)
        H%DProf(i)=H%DProf(i)/mscale
        H%D1Prof(i)=H%D1Prof(i)/mscale
        H%D2Prof(i)=H%D2Prof(i)/mscale
c        print*, "hmmm",i,H%RProf(i),H%DProf(i),H%D1Prof(i),H%D2Prof(i)
      enddo

      close(10)
      return
      end subroutine

ccccccc



      end module
