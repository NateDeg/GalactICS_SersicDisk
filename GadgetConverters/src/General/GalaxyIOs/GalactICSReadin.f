cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalactICSInMod
      use GalObjDef
      implicit none

      contains

ccccc
      subroutine ReadGalactICSFile(fname,G)
      implicit none
      integer i
      character(*) fname
      Type(Galaxy) G
c
      open(10,file=fname,status='old')      !Open file
      read(10,*) G%nPart                    !Get the number of particles
      call AllocateGalaxy(G)                !Allocate all the arrays
      do i=1,G%nPart                        !Read Everything in
        read(10,*)    G%P(i)%Mass,G%P(i)%Pos,G%P(i)%Vel
        G%P(i)%ID=i
      enddo
      close(10)

      return
      end subroutine
ccccccc

      end module

