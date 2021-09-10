cccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to shift the position and
c       velocity of all particles in some 'Galaxy' object
ccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalShiftMod
      use GalObjDef
      use CartGalConvert


      implicit none
      contains
cccccc
      subroutine GalShiftPosByAng(G,PosShift,AngShift)
      implicit none
      Type(Galaxy) G
      real PosShift(3),AngShift(3)


      call GalactoToCart(PosShift,AngShift)
      call GalPosShift(G,PosShift)

      return
      end subroutine
cccccccccc


cccccccccccccc
      subroutine GalPosShift(G,PosShift)
c
      implicit none
c
      Type(Galaxy) G
      integer i
      real PosShift(3)
c
ccccccccc
      print*, "Shifting Gal Pos",PosShift

      do i=1, G%nPart
        G%P(i)%Pos(1:3)= G%P(i)%Pos(1:3)+PosShift(1:3)
      enddo
      return
      end subroutine
cccccccccc


cccccccccccccc
      subroutine GalVelShift(G,VelShift)
c
      implicit none
c
      Type(Galaxy) G
      integer i
      real VelShift(3)
c
ccccccccc
      print*, "Shifting Gal Vel",VelShift

      do i=1, G%nPart
        G%P(i)%Vel(1:3)= G%P(i)%Vel(1:3)+VelShift(1:3)
      enddo
      return
      end subroutine
cccccccccc


      end module
