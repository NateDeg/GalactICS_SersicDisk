cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains declarations for all globals
c     used in GalactICS
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module ConvertersGlobs
      use GalObjDef


      implicit none

      integer numGals,nCompMax
      integer,ALLOCATABLE :: nComponents(:)
      character(200),ALLOCATABLE :: CompFileName(:,:)
     &                  ,CenteredGalOutfiles(:)
      character(200) CombineOutFile,CompSeparateFile,GalactICSPath
      integer,ALLOCATABLE :: PartType(:,:)

      real,ALLOCATABLE :: EulerAngs(:,:), COM(:,:),COV(:,:)

      Type(Galaxy) CombinedGal
      Type(Galaxy),ALLOCATABLE :: Gals(:),GalComps(:,:)
      Type(Galaxy)  PartTypeGal(0:5)
      integer numPerPartType(0:5),numCompPerPartType(0:5)
      integer,ALLOCATABLE :: PartTypeOrder(:,:),UseSwitch(:,:)
      integer,ALLOCATABLE :: ComponentParticleOrder(:,:)

      end module

