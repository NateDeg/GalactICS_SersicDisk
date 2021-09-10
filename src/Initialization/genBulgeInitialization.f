cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module initializes the various components and
c       constants used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module iniGenBulgeMod
      use GenCompGlobs
      use Globs
      use BulgeDFMod
      use BulgeGridDensMod


      implicit none

      contains

cccccccc
      subroutine IniGenBulge()
      implicit none

      print*, "Initialize Gen Halo"
      rtrunc=bulgeedge
      pMass=bulgemass/real(nPart)
      sig2=DF%psi0
      u1max=rtrunc
      v1max=0.5*Pi

      fcut_bulge=dfbulge(DF%psic)
      print*, "Constants", rtrunc,pMass,sig2,u1max,v1max,fcut_bulge

      ALLOCATE(P(nPart))
      potential=0.
      kinetic=0.
      call FindRhoMaxB()

      return
      end subroutine
ccccccc

cccccc
      subroutine FindRhoMaxB()
      implicit none
      integer i
      real rcyl,z
      real dr,rhocur,rhomax1
      integer nSteps
c
      rhomax1=0.
      nSteps=10000
      dr =rtrunc/real(nSteps)
      do i=1,nSteps
        rcyl=real(i)*dr
        z=0.
        rhocur=rcyl*rcyl*bulgedens(rcyl,z)
        if(rhocur .gt. rhomax1) rhomax1=rhocur
c        print*, "hmmm ini", rcyl,z,rhocur,rhomax1
      enddo
      do i=1,nSteps
        z=real(i)*dr
        rcyl=0.
        rhocur=rcyl*rcyl*bulgedens(rcyl,z)
        if(rhocur .gt. rhomax1) rhomax1=rhocur
c        print*, "hmmm ini 2", rhocur,rhomax1
      enddo
      print*, "rhomax1=", rhomax1
      rhomax=1.5*rhomax1
      rhomin=1.e-10*rhomax

c

      return
      end subroutine
cccccc


      end module
