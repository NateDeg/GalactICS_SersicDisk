cccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to shift the position and
c       velocity of all particles in some 'Galaxy' object
ccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalUnitsMod
      use GalObjDef


      implicit none
      contains
cccccccccccccc
      subroutine GalMassConv(G,MConv)
c
      implicit none
c
      Type(Galaxy) G
      integer i
      real MConv
c
ccccccccc
      do i=1, G%nPart
        G%P(i)%Mass= G%P(i)%Mass*MConv
      enddo
      return
      end subroutine
cccccccccc


cccccccccccccc
      subroutine GalVelConv(G,VConv)
c
      implicit none
c
      Type(Galaxy) G
      integer i
      real VConv
c
ccccccccc

      do i=1, G%nPart
        G%P(i)%Vel(1:3)= G%P(i)%Vel(1:3)*VConv
      enddo
      return
      end subroutine
cccccccccc


      end module
