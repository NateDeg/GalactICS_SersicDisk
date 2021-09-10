cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains routines for combining or
c       separating different galaxy components
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalCompAddMod
      use GalObjDef
      implicit none

      contains

ccccc
      subroutine GalCompCombine(GFin,nComp,GIni)
      implicit none
      integer nComp
      Type(Galaxy) GFin,GIni(nComp)
      integer i,j,k
c
      print*, "Add components together"

      GFin%nPart=sum(Gini(1:nComp)%nPart)
      do i=0,5
        GFin%nPartType(i)=sum(Gini(1:nComp)%nPartType(i))
      enddo
c      print*, GFin%nPart
      call AllocateGalaxy(GFin)

      k=0
      do i=1, nComp
        do j=1, Gini(i)%nPart
            k=k+1
            GFin%P(k)=Gini(i)%P(j)
            GFin%P(k)%ID=k
        enddo
c        print*, "Test",i,j,k
      enddo


      return
      end subroutine
ccccccc

      end module

