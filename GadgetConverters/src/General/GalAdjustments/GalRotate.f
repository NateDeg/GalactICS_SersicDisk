cccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to calculate the moments 
c     of some a passed array of n-body particles per 
c     pixel element
c
ccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalRotMod
      use GalObjDef
      use EulerRotations


      implicit none
      contains
cccccccccccccc
      subroutine GalRotate(G,AngTemp)
c
      implicit none
c
      Type(Galaxy) G
      integer i
      real AngTemp(3),Temp(3)
c
ccccccccc
      print*, "Rotating Gal", AngTemp

c      call CommonRotMatCalc(AngTemp)
      call CommonRotMatCalc_ZYZ(AngTemp)
      do i=1, G%nPart
        call EulerRot_CR(G%P(i)%Pos(1:3),Temp(1:3))
        G%P(i)%Pos(1:3)=Temp(1:3)
        call EulerRot_CR(G%P(i)%Vel(1:3),Temp(1:3))
        G%P(i)%Vel(1:3)=Temp(1:3)
      enddo


      return
      end subroutine
cccccccccc

      end module
