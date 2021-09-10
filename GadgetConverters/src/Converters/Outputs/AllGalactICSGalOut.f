cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module AllGalactICSGalOutMod
      use ConvertersGlobs
      use GalactICSOutMod

      implicit none

      contains

ccccc
      subroutine AllGalactICSGalOutput()
      implicit none
      integer i

      print*, "writing out each galaxy"
c       Start by Allocating enough 'Galaxy Obj' components
      do i=1, numGals
        call WriteGalactICSFile(CenteredGalOutfiles(i),Gals(i))
      enddo


      return
      end subroutine
ccccccc


ccccc
      subroutine FinalGalactICSGalOutput()
      implicit none
      integer i

      print*, "writing out final combined galaxy"
c       Start by Allocating enough 'Galaxy Obj' components
      call WriteGalactICSFile(CombineOutFile,CombinedGal)
      return
      end subroutine
ccccccc


      end module

