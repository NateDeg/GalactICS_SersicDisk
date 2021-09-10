cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalCentInMod
      use ParticleCompReadInObj
      implicit none

      contains

cccccc
      subroutine ReadGalCentIn(infile,nIter,P)
      implicit none
      character(*) infile
      Type(PartInObj) P
      integer i,nIter
cccc

      open(10,file=infile,status='old')
      read(10,*)
      read(10,*) nIter

      P%IDTypeSwitch=1

                  !Select by Component ID
      read(10,*)
      read(10,*) P%nComp
      call AllocateComps(P)

      read(10,*)
      do i=1, P%nComp
        read(10,*) P%CompID(i,1),P%CompID(i,2),P%MLComp(i)
      enddo

      close(10)
      return
      end subroutine
cccccc

      end module
