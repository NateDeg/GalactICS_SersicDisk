cccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Angular Distance Mod
c
c     This modules contains routines that calculate the 
c     angular distance between two points in angular coordinates
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      module AngDistMod
c
      implicit none

      contains
ccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     AngDist
c
c     This routine calculates the angular distance between
c     two points
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine AngDist(Ang1,Ang2,Dist)
c
      implicit none
c
      real Ang1(2),Ang2(2),Dist
      real term1,term2,term3
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc

      term1=sin(Ang1(2))*sin(Ang2(2))
      term2=cos(Ang1(2))*cos(Ang2(2))
      term3=cos(Ang1(1)-Ang2(1))
      Dist=acos(term1+term2*term3)
      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     AngDist
c
c     This routine calculates the angular distance between
c     two points
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine AngDist2(Ang1,Ang2,Dist)
c
      implicit none
c
      real Ang1(2),Ang2(2),Dist
      real term1,term2,term3
      real dLong,dLat
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dLong=Ang1(1)-Ang2(1)
      dLat=Ang1(2)-Ang2(2)
      term1=(sin(dLat/2.))**2.
      term2=(sin(dLong/2.))**2.
      term3=cos(Ang1(2))*cos(Ang2(2))

      Dist=term1+term2*term3
      Dist=2.*asin(sqrt(Dist))

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccccccccccc










cccccccccccccccccccccccccccccccccccccccccccccccc
c

      end module
