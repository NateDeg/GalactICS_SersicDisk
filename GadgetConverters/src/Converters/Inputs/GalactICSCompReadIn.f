cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalactICSCompInMod
      use ConvertersGlobs
      use GalactICSInMod
      use RandomMod

      implicit none

      contains

ccccc
      subroutine AllGalactICSCompsReadIn()
      implicit none
      integer i

      print*, "Reading in the various GalactICS Components"
c       Start by Allocating enough 'Galaxy Obj' components
      ALLOCATE(Gals(numGals))
      ALLOCATE(GalComps(numGals,nCompMax))
c       Read in all the files associated with each galaxy
c
      do i=1,numGals
        call FullGalactICSGalaxyRead(i)
      enddo

      call SetUpFinalOrders()


      return
      end subroutine
ccccccc



cccc
      subroutine FullGalactICSGalaxyRead(GalID)
      implicit none
      integer i,GalID,np

      do i=1,nComponents(GalID)
c        print*, CompFileName(GalID,i)
        call ReadGalactICSFile(CompFileName(GalID,i),GalComps(GalID,i))     !Read in Each component
        np=GalComps(GalID,i)%nPart
        GalComps(GalID,i)%P(1:np)%PartType=PartType(GalID,i)            !Set a number of possible values to 0
        GalComps(GalID,i)%P(1:np)%Temp=0.
        GalComps(GalID,i)%P(1:np)%Entropy=0.
        GalComps(GalID,i)%P(1:np)%Rho=0.
        GalComps(GalID,i)%P(1:np)%NE=0.
        GalComps(GalID,i)%nPartType(0:5)=0
        GalComps(GalID,i)%nPartType(PartType(GalID,i))=np

c        if(PartType(GalID,i) .eq. 3) then
c            call ShelledCOM(GalComps(GalID,i))
c        endif
c        call ShelledCOM(GalComps(GalID,i))
c        call GasFlipZ(GalComps(GalID,i))
        if(PartType(GalID,i) .eq. 0) then
            call GasAdjustVZEntropy(GalComps(GalID,i))      !For gas, set entropy term to vz and vz to zero
c        call GasFlipZ(GalComps(GalID,i))
        endif
      enddo
c      print*, "Quick Read in test"
c     &          , GalComps(GalID,1:nComponents(GalID))%nPart

      return
      end subroutine
ccccccc

cccccc
      subroutine GasAdjustVZEntropy(G)
      implicit none
      Type(Galaxy) G
      G%P(1:G%nPart)%Entropy=(G%P(1:G%nPart)%Vel(3)*100.)**2.
      G%P(1:G%nPart)%Vel(3)=0.
      return
      end subroutine
ccccccc


cccccc
      subroutine GasFlipZ(G)
      implicit none
      Type(Galaxy) G
      integer i, idum
      real RandNum

      idum=-1
      do i=1, G%nPart
c        RandNum=ran3(idum)
c        if(RandNum .le. 0.5) G%P(i)%Pos(3)=-G%P(i)%Pos(3)
        G%P(i)%Pos(3)=-G%P(i)%Pos(3)
        G%P(i)%Vel(3)=-G%P(i)%Vel(3)
      enddo

      return
      end subroutine
ccccccc

cccccccc
      subroutine SetUpFinalOrders()
      implicit none
      integer i,j,PType
      integer Tot,Count,Psum

      Tot=sum(nComponents(1:numGals))
      ALLOCATE(PartTypeOrder(Tot,2))
      ALLOCATE(ComponentParticleOrder(Tot,2))
      ALLOCATE(UseSwitch(numGals,nCompMax))
      UseSwitch=0
      Count=0
      numPerPartType=0
      Psum=0
      do PType=0,5
        do i=1,numGals
            do j=1,nComponents(i)
                if(PartType(i,j) .eq. PType) then
                    if(UseSwitch(i,j) .eq. 0) then
                        Count=Count+1
                        UseSwitch(i,j)=1
                        PartTypeOrder(Count,1)=i
                        PartTypeOrder(Count,2)=j
                        numPerPartType(PType)=numPerPartType(PType)
     &                          +GalComps(i,j)%nPart
                        numCompPerPartType(PType)=
     &                          numCompPerPartType(PType)+1
                        ComponentParticleOrder(Count,1)=Psum+1
                        PSum=PSum+GalComps(i,j)%nPart
                        ComponentParticleOrder(Count,2)=Psum
                        print*,Count,i,j
     &                          ,ComponentParticleOrder(Count,1:2)

                    endif
                endif
            enddo
        enddo
      enddo


      return
      end subroutine


ccccc
      subroutine ShelledCOM(G)
      Type(Galaxy) G
      integer i, j,nShells
      parameter(nShells=20)
      real dShell
      parameter(dShell=1.)

      real COM(nShells,3), R
      integer nPerShell(nShells)

      COM=0.
      nPerShell=0.

      do i=1, G%nPart
        R=sqrt(G%P(i)%Pos(1)**2.+G%P(i)%Pos(2)**2.+G%P(i)%Pos(3)**2.)
        j=int(R/dShell)+1
        if( j.le. nShells) then
            nPerShell(j)=nPerShell(j)+1
            COM(j,1:3)=COM(j,1:3)+G%P(i)%Pos(1:3)
        endif
      enddo

      do i=1, nShells
        COM(i,1:3)=COM(i,1:3)/real(nPerShell(i))
        print*, i, (i*dShell), COM(i,1:3)
      enddo

      do i=1, G%nPart
        R=sqrt(G%P(i)%Pos(1)**2.+G%P(i)%Pos(2)**2.+G%P(i)%Pos(3)**2.)
        j=int(R/dShell)+1
        if( j.le. nShells) then
            G%P(i)%Pos(3)=G%P(i)%Pos(3)-COM(j,3)
        endif
      enddo


      return
      end subroutine

      end module

