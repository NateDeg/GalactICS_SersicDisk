cccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to calculate the moments 
c     of some a passed array of n-body particles per 
c     pixel element
c
ccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalCentMod
      use GalObjDef
      use CentOfMassMod


      implicit none
      contains
cccccccccccccc
      subroutine GalCent(G,nIterCOM)
c
      implicit none
      integer i,nIterCOM
      real,ALLOCATABLE :: PTemp(:,:),VTemp(:,:),MTemp(:)
      Type(Galaxy) G
ccccccccc
      print*, "Centering Galaxy in Cartesian Space"
      i=1
      ALLOCATE(PTemp(3,G%nPart))
      ALLOCATE(VTemp(3,G%nPart))
      ALLOCATE(MTemp(G%nPart))
      do i=1,G%nPart
        PTemp(1:3,i)=G%P(i)%Pos(1:3)
        VTemp(1:3,i)=G%P(i)%Vel(1:3)
        MTemp(i)=G%P(i)%Mass
      enddo

      call IterCentMassVelAdjust(G%nPart,PTemp,VTemp,MTemp,nIterCOM)

      do i=1,G%nPart
        G%P(i)%Pos(1:3)=PTemp(1:3,i)
        G%P(i)%Vel(1:3)=VTemp(1:3,i)
        G%P(i)%Mass=MTemp(i)
      enddo

      return
      end subroutine
cccccccccc



cccccccccccccc
      subroutine CentGalOnComp(G,GCent,nIterCOM)
c
      implicit none
      integer i,nIterCOM
      real,ALLOCATABLE :: PTemp(:,:),VTemp(:,:),MTemp(:)
      Type(Galaxy) G,GCent
      real COM(3),COV(3)
ccccccccc
      print*, "Centering Galaxy in Cartesian Space on Comp",nIterCOM
     &              ,GCent%nPart
      i=1
      ALLOCATE(PTemp(3,GCent%nPart))
      ALLOCATE(VTemp(3,GCent%nPart))
      ALLOCATE(MTemp(GCent%nPart))
      do i=1,GCent%nPart
        PTemp(1:3,i)=GCent%P(i)%Pos(1:3)
        VTemp(1:3,i)=GCent%P(i)%Vel(1:3)
        MTemp(i)=GCent%P(i)%Mass
      enddo

      nIterCOM=3
      call IterCentMass(GCent%nPart,PTemp,VTemp,MTemp,nIterCOM,COM,COV)
      print*, "COM of Comp",COM,COV,nIterCOM


      do i=1,G%nPart
        G%P(i)%Pos(1:3)=G%P(i)%Pos(1:3)-COM(1:3)
        G%P(i)%Vel(1:3)=G%P(i)%Vel(1:3)-COV(1:3)
c        print*, "hmmm", i,G%P(i)%Pos(1:3)
c        print*, G%P(i)%Mass
c        G%P(i)%Mass=MTemp(i)
      enddo

      return
      end subroutine
cccccccccc


      end module
