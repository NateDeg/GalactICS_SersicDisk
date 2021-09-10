cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains routines that combine each galaxies individual
c       components together...or separates them
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module MoveGalsMod
      use ConvertersGlobs
      use GalRotMod
      use GalShiftMod
      implicit none

      contains
cccccc
      subroutine MoveAllGals()
      implicit none
      integer i,j

      print*, "Move All non-centered galaxies"
      do i=2,numGals
        do j=1,nComponents(i)
        print*, i
            call GalRotate(GalComps(i,j),EulerAngs(1:3,i-1))          !Rotate
            call GalPosShift(GalComps(i,j),COM(1:3,i-1))              !Move to new position
            call GalVelShift(GalComps(i,j),COV(1:3,i-1))              !Move to new velocity
        enddo
      enddo


      return
      end subroutine
ccccccc

      end module

