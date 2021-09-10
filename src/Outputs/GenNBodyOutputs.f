cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module outputs the halo particles made in GenHalo.f
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module outNBodyMod
      use GenCompGlobs


      implicit none

      contains


ccccccccc
      subroutine OutputNBodyFile(fname)
      implicit none
      character(30) fname
      integer i
      open(10,file=trim(fname),status='replace')
      write(10,*) nPart, 0.
      do i=1,nPart
        write(10,*) P(i)%Mass,P(i)%Pos(1:3),P(i)%Vel(1:3)
      enddo
      close(10)

      return
      end subroutine

ccccccc

      end module

