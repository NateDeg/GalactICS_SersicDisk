cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to center the
c     angular coordinates for the Galaxy and convert them
c     to arcseconds
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalObsCentMod
      use CommonConsts
      use GalObjDef

      implicit none
      contains
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine GalObsCent(G,lTarg,bTarg)
      implicit none
      integer i
      Type(Galaxy) G
      real lTarg,bTarg
      
      
ccccccccccccccccccccccccccccccccccccccccccccccccccc

c      print*,"size in l",
c     &     minval(AngPos(2,1:nPart))*Rad_Deg
c     &     ,maxval(AngPos(2,1:nPart))*Rad_Deg
c      print*,"size in b",
c     &     minval(AngPos(3,1:nPart))*Rad_Deg,
c     &     maxval(AngPos(3,1:nPart))*Rad_Deg

c      print*, "Centering and converting to arcseconds"
      do i=1, G%nPart
        G%P(i)%AngPos(2)=G%P(i)%AngPos(2)-lTarg
        G%P(i)%AngPos(3)=G%P(i)%AngPos(3)-bTarg
        G%P(i)%AngPos(2)=G%P(i)%AngPos(2)
     &              *RadToDeg*DegToArcMin*ArcMinToArcSec
        G%P(i)%AngPos(3)=G%P(i)%AngPos(3)
     &              *RadToDeg*DegToArcMin*ArcMinToArcSec
      enddo
      
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccc

      end module
