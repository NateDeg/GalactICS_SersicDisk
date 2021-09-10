cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains declarations for all globals
c     used in GalactICS
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GenCompGlobs
      use PartObjDef

      implicit none


      real StreamingFrac
      integer nPart, Seed
      integer idum
      integer icofm
      integer nrsave
      real drsave

      real diskmass,diskedge
      real bulgemass,bulgeedge
      real halomass, haloedge
      real gasdiskmass,gasdiskparticlemass

      real pMass, sig2,u1max,v1max,fcut_halo,fcut_bulge
      real rtrunc

      Type(Particle),ALLOCATABLE :: P(:)

      real potential,kinetic
      real potentialz,kineticz
      real rhomax,rhomin

      logical OriBulgeFlag,OriDiskFlag1,OriDiskFlag2
      logical OriHaloFlag

      integer OmekapCallCount, RCircCallCount

      character(20) OutputFileName,inFileName
      integer DiskSwitch,Adio_IsoTempSwitch

      real broadcon
      real umin
      real time

      end module

