ccccccccccc
c
c     Get Force Routines
c
c       This module contains the various routines for
c       calculating Force for the various galaxy components
cccccccccc
      module GetForceMod
      use Globs
      use SersicProfilesMod
      implicit none

      contains



cccccccc
      real function gettotalforce(rad)
      implicit none
      real rad
      gettotalforce = 0.
      if(DiskFlag1) then
        gettotalforce = gettotalforce + getDPFrGridForce(rad,D1%DPFr)
c        print*, "Disk1 Force Test", gettotalForce
c     &              ,getDPFrGridForce(rad,D1%DPFr)
      endif
      if(DiskFlag2) then
        gettotalforce = gettotalforce + getDPFrGridForce(rad,D2%DPFr)
c        print*, "Disk2 Force Test", gettotalForce
c     &              ,getDPFrGridForce(rad,D2%DPFr)
      endif
      if(GasFlag) then
        gettotalforce = gettotalforce + getDPFrGridForce(rad,Gas%DPFr)
c        print*, "Gas Force Test", gettotalForce
c     &              ,getDPFrGridForce(rad,Gas%DPFr)
      endif
      if(BulgeFlag) then
        gettotalforce = gettotalforce + sersicforce(rad,Bulge) !(see Bulge/sersicprofiles.f)
c        print*, "Bulge Force Test", gettotalForce
c     &              ,sersicforce(rad,Bulge)
      endif
      if(HaloFlag) then
        gettotalforce = gettotalforce + getDPFrGridForce(rad,Halo%DPFr)
c        print*, "Halo Force Test", gettotalForce
c     &              ,getDPFrGridForce(rad,Halo%DPFr)
      endif



      return
      end function
cccccccccc


ccccccccc
      real function getDPFrGridForce(r,Grid)
      implicit none
      real r, r1,r2
      real t,tm1
      integer ihi
      Type(DPFrObj) Grid

      ihi=int(r/Grid%dr)+1
      if (ihi.lt.1) ihi=1
      if (ihi.gt.Grid%nr) ihi=Grid%nr
      r1=Grid%dr*(real(ihi)-1)
      r2=Grid%dr*real(ihi)
      t=(r-r1)/(r2-r1)
      tm1 = 1.0 - t
      getDPFrGridForce = (t*Grid%fr(ihi)+ tm1*Grid%fr(ihi-1))
      return
      end function
c
ccccccc

      end module



