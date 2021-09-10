cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module initializes the various components and
c       constants used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module iniGenHaloMod
      use GenCompGlobs
      use Globs
      use HaloDFMod
      use HaloGridDensMod

      implicit none

      contains

cccccccc
      subroutine IniGenHalo()
      implicit none

      print*, "Initialize Gen Halo"
      rtrunc=haloedge
      pMass=halomass/real(nPart)
      sig2=DF%psi0
      u1max=rtrunc
      v1max=0.5*Pi

      fcut_halo=dfhalo(DF%psic)
      print*, "Constants", rtrunc,pMass,sig2,u1max,v1max,fcut_halo

      ALLOCATE(P(nPart))
      potential=0.
      kinetic=0.
      call FindRhoMax()

      return
      end subroutine
ccccccc

cccccc
      subroutine FindRhoMax()
      implicit none
      integer i
      real rcyl,z
      real dr,rhocur,rhomax1
c
      rhomax1=0.
      dr =rtrunc/100.
      do i=1,100
        rcyl=real(i)*dr
        z=0.
        rhocur=rcyl*rcyl*halodens(rcyl,z)
        if(rhocur .gt. rhomax1) rhomax1=rhocur
c        print*, "hmmm ini", rcyl,z,rhocur,rhomax1
      enddo
      do i=1,100
        z=real(i)*dr
        rcyl=0.
        rhocur=rcyl*rcyl*halodens(rcyl,z)
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
