ccccccccccc
c
c     Get Psi Routines
c
c       This module contains the various routines for
c       calculating psi for the various galaxy components
cccccccccc
      module GetPsiMod
      use Globs
      use SersicProfilesMod
      use BHGeneralMod
      implicit none

      contains



cccccccc
      real function gettotalpsi(rad)
      implicit none
      real rad
      gettotalpsi = 0.

c      print*, "Getting Total Psi at r=", rad,DiskFlag1,DiskFlag2
c     &          ,GasFlag,BulgeFlag,HaloFlag
      if(DiskFlag1) then
         gettotalpsi = gettotalpsi + getDPFrGridPsi(rad,D1%DPFr)
c        print*, "Disk1 Psi Test", gettotalpsi
c     &              ,getDPFrGridPsi(rad,D1%DPFr)
      endif
      if(DiskFlag2) then
         gettotalpsi = gettotalpsi + getDPFrGridPsi(rad,D2%DPFr)
c        print*, "Disk2 Psi Test", gettotalpsi
c     &                  ,getDPFrGridPsi(rad,D2%DPFr)
      endif
      if(GasFlag) then
         gettotalpsi = gettotalpsi + getDPFrGridPsi(rad,Gas%DPFr)
c        print*, "Gas Psi Test", gettotalpsi
c     &                  ,getDPFrGridPsi(rad,Gas%DPFr)
      endif
      if(BulgeFlag) then
         gettotalpsi = gettotalpsi + sersicpot(rad,Bulge)  !Bulge Potential (see Bulge/sersicprofiles.f)
c        print*, "Bulge Psi Test", gettotalpsi,sersicpot(rad,Bulge)
      endif
      if(HaloFlag) then
         gettotalpsi = gettotalpsi + getDPFrGridPsi(rad,Halo%DPFr)
c        print*, "Halo Psi Test", rad,gettotalpsi
c     &              ,getDPFrGridPsi(rad,Halo%DPFr)
c        print*, Halo%DPFr%Pot(1)
      endif

      if(BHFlag) then
        gettotalpsi = gettotalpsi + getbhpsi(rad)
c        print*, "Black Hole Psi Test", gettotalpsi,getbhpsi(rad,Bulge)
      endif

c      print*, "get total psi", rad, gettotalpsi
      return
      end function
cccccccccc


ccccccccc
      real function getDPFrGridPsi(r,Grid)
      implicit none
      real r, r1,r2
      real t,tm1
      integer ihi
      Type(DPFrObj) Grid
      real test

c      print*, "Grid Ini",r,Grid%nr,Grid%dr,Grid%pot(0)
c     &              ,Grid%pot(Grid%nr)

      ihi=int(r/Grid%dr)+1
      if (ihi.lt.1) ihi=1
      if (ihi.gt.Grid%nr) ihi=Grid%nr
      r1=Grid%dr*(real(ihi)-1)
      r2=Grid%dr*real(ihi)
      t=(r-r1)/(r2-r1)
      tm1 = 1.0 - t
      test= (t*Grid%pot(ihi)+ tm1*Grid%pot(ihi-1))
      getDPFrGridPsi = test
c      print*, "DPFrGrid Test", r,ihi, r1,r2
c     &          ,Grid%pot(ihi),Grid%pot(ihi-1),test
      return
      end function
c
      end module



