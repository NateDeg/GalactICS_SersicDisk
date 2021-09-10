      module ParticleCompReadInObj
      implicit none

      Type PartInObj
        integer nComp,CompTot,nTypes,IDTypeSwitch
        integer,ALLOCATABLE :: CompID(:,:),PLims(:,:)
        integer,ALLOCATABLE :: TargPTypes(:)
        integer,ALLOCATABLE :: CompIDT(:,:)
     &                      ,PLimsT(:,:),PTypeT(:)
        real,ALLOCATABLE :: MLComp(:),MLType(:)
      end Type

      contains
cccc
      subroutine AllocateComps(P)
      implicit none
      Type(PartInObj) P
      ALLOCATE(P%CompID(P%nComp,2))
      ALLOCATE(P%PLims(P%nComp,2))
      ALLOCATE(P%MLComp(P%nComp))
      return
      end subroutine
ccc
      subroutine AllocateTypes(P)
      implicit none
      Type(PartInObj) P
      ALLOCATE(P%TargPTypes(P%nTypes))
      ALLOCATE(P%MLType(P%nTypes))
      return
      end subroutine
cccc
      subroutine AllocateTotals(P)
      implicit none
      Type(PartInObj) P
      ALLOCATE(P%CompIDT(P%CompTot,2))
      ALLOCATE(P%PLimsT(P%CompTot,2))
      ALLOCATE(P%PTypeT(P%CompTot))
      return
      end subroutine
cccccc


      end module
