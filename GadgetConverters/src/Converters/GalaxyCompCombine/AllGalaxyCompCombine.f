cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains routines that combine each galaxies individual
c       components together...or separates them
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module ConvertersGalCompCombine
      use ConvertersGlobs
      use GalCompAddMod
      implicit none

      contains
cccccc
      subroutine AllGalCombine()
      implicit none
      integer i 

      print*, "Combine all galaxy components into larger arrays"
      do i=1, numGals
        call GalCompCombine(Gals(i), nComponents(i)
     &          ,GalComps(i,1:nComponents(i)))
      enddo

      return
      end subroutine
ccccccc

cccccc
      subroutine CombineFinalGal()
      implicit none
      integer i,j,Tot,Count,Ptype
      Type(Galaxy),ALLOCATABLE :: CompGals(:)


      Count=0
      do PType=0,5
        print*, PType
        if(numPerPartType(PType) .ge. 1) then
            ALLOCATE(CompGals(numCompPerPartType(PType)))
            do i=1,numCompPerPartType(PType)
                Count=Count+1
                CompGals(i)=GalComps(PartTypeOrder(Count,1)
     &                      ,PartTypeOrder(Count,2))
            enddo
        call GalCompCombine(PartTypeGal(PType),numCompPerPartType(PType)
     &          ,CompGals(1:numCompPerPartType(PType)))
            DEALLOCATE(CompGals)
        else
            PartTypeGal(PType)%nPart=0
        endif
c        print*, "Fast Test", PType,PartTypeGal(PType)%nPart
      enddo


      call GalCompCombine(CombinedGal, 6
     &          ,PartTypeGal(0:5))
c      print*, "Hmmm", CombinedGal%nPartType

      return
      end subroutine
ccccccc

      end module

