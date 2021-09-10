cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines for converting the 
c     cartesian coordinates of the galaxy to (l,b,d,vr) coords
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalCartToObserve
      use GalObjDef
      use CartGalConvert
      use SolarParams

      implicit none


      contains

ccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine CalcGalObs(G)
c
      implicit none
c
      integer i
      Type(Galaxy) G

cccccccccccccccccccccccccccccccccccccc
c      print*, "Converting Galaxy To Observable Phase Space"
      do i=1,G%nPart
c      do i=1078,1078
        call CartToGalacto(G%P(i)%AngPos,G%P(i)%Pos)
        call CartVelToGalacto(G%P(i)%AngPos,G%P(i)%Vel,G%P(i)%AngVel)
c        print*, "Cart",i,G%P(i)%Pos,G%P(i)%Vel
c        print*, "Ang",i,G%P(i)%AngPos(1),G%P(i)%AngPos(2:3)*RadToDeg
c     &                  ,G%P(i)%AngVel
c        print*,G%P(i)%AngPos,G%P(i)%Vel,G%P(i)%AngVel
      enddo

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      end module
