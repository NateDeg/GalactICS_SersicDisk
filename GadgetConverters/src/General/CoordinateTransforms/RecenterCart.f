cccccccccccccccccccccccccccccccccccccccc
c
c     This module contains a routine to recenter
c     Cartesian points on some new center
c
cccccccccccccccccccccccccccccccccccccccc

      module RecenterMod
      contains

cccccccccccccccccccccccccccccccccccccccccccccc
c
c     Recenter
c
c     This subroutine shifts coordinates to a new center
c
ccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine recenter(Pos, Shift)
c
      implicit none
c
      real Pos(3)
      real Shift(3)
c
cccccccccccccccccccccccccccccccccccccccccccccccc
c
      Pos(1)=Pos(1)-Shift(1)
      Pos(2)=Pos(2)-Shift(2)
      Pos(3)=Pos(3)-Shift(3)
      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Shift Array
c     
c     This array runs recenter on an array
c
cccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine ShiftArray(nPart,Pos,Shift)
c
      implicit none
      integer i, nPart
      real Pos(3,nPart), Shift(3)
cccccccccccccccccccccccccccccccccccccccccccccc

      do i=1,nPart
         call recenter(Pos(1:3,i),Shift)
      enddo
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccc

      end module
