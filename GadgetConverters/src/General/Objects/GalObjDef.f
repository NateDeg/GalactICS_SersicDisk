cccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to calculate the moments 
c     of some a passed array of n-body particles per 
c     pixel element
c
ccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalObjDef
      implicit none

      Type Particle
        real Mass,ML,Luminosity
        real Pos(3),Vel(3)
        real AngPos(3),AngVel(3)
        integer CellID(3),ID
        integer PartType
        real Temp,Entropy
        real Rho,NE
        real CentFreq,CentLambda
        real Accel(3),AngAccel(3),dEntropy,Smooth,Pot
      end Type

      Type Galaxy
        integer nPart,nPartType(0:5)
        Type(Particle),ALLOCATABLE :: P(:)
        integer SFR,Feedback,Cooling
        real Time,Redshift,Omega0,OmegaLambda,Hubble,Box
      end Type

      contains
ccccccc
      subroutine AllocateGalaxy(G)
c
      implicit none
      Type(Galaxy) G

      ALLOCATE(G%P(G%nPart))

      return
      end subroutine
cccccccc

ccccccc
      subroutine DeAllocateGalaxy(G)
c
      implicit none
      Type(Galaxy) G

      DEALLOCATE(G%P)

      return
      end subroutine
cccccccc



      end module
