cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains writes out a GalactICS style file
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalactICSOutMod
      use GalObjDef
      implicit none

      contains

ccccc
      subroutine WriteGalactICSFile(fname,G)
      implicit none
      integer i
      character(*) fname
      Type(Galaxy) G
c
      open(10,file=fname,status='replace')      !Open file
      write(10,*) G%nPart,0.                    !Write the number of particles
      do i=1,G%nPart                        !Read Everything in
        write(10,*)    G%P(i)%Mass,G%P(i)%Pos,G%P(i)%Vel
      enddo
      close(10)

      return
      end subroutine
ccccccc

      end module

