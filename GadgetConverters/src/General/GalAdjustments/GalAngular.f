cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines for converting the 
c     cartesian coordinates of the galaxy to (l,b,d,vr) coords
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalCartToAng
      use GalObjDef
      use CartAngleConvert
      use SolarParams

      implicit none


      contains

ccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine CalcGalAng(G)
c
      implicit none
c
      integer i
      Type(Galaxy) G

cccccccccccccccccccccccccccccccccccccc
      print*, "Converting Galaxy To Angular Coords"
c      do i=1,10
      do i=1,G%nPart
        call CartToAng(G%P(i)%Pos,G%P(i)%AngPos)
        call CartToAngVel(G%P(i)%AngPos,G%P(i)%Vel,G%P(i)%AngVel)
c        print*,G%P(i)%ID,G%P(i)%Pos,G%P(i)%Vel
c        print*,G%P(i)%ID,G%P(i)%AngPos,G%P(i)%AngVel
c     &          ,G%P(i)%Entropy,G%P(i)%Temp
      enddo

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine CalcGalAngAccel(G)
c
      implicit none
c
      integer i
      Type(Galaxy) G

      do i=1,G%nPart
        call CartToAngVel(G%P(i)%AngPos,G%P(i)%Accel
     &              ,G%P(i)%AngAccel)
      enddo
      return
      end subroutine
ccccccccc


      end module
