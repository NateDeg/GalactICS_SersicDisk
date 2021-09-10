ccccccccccccc
c
c     Disk Density Module
c
c       This module contains routines for calculating the disk density
c
c       contains diskdensestimate, diskvertdens, disksurfdens, dpolardiskdens
c
c
cccccccccccc
c
      module TotDensMod
      use Globs
      use DiskDensMod
      use HaloDensityMod
      use SersicProfilesMod

      implicit none
      contains
c
      real function TotDens_NoGas(r,z)
      implicit none
      real dsk1,dsk2,hdens,bdens
      real r,z,rr

      rr=sqrt(r*r+z*z)
      hdens=halodensity(rr)     !There's always a halo  (see Halo/halodensity.f)

      if(DiskFlag1) then
        dsk1=diskdensestimate(r,z,D1)       !1st Disk (see Disk/diskdensity.f)
      else
        dsk1=0.
      endif
      if(DiskFlag2) then
        dsk2=diskdensestimate(r,z,D2)       !2nd Disk (see Disk/diskdensity.f)
      else
        dsk2=0.
      endif
      if(bulgeflag) then
        bdens = sersicdens(rr,Bulge)        !Bulge (see Bulge/sersicprofiles.f)
      else
        bdens = 0.
      endif
      TotDens_NoGas=dsk1+dsk2+hdens+bdens   !Total
c      print*, "Tot Dens, No Gas Test", r,z
c     &          ,TotDens_NoGas,dsk1,dsk2,hdens,bdens

      return
      end function
cccccccccccccc

cccccccccc
      real function TotDens_Grid(r,z)
      implicit none
      real dsk1,dsk2,hdens,bdens,gdens
      real r,z,rr

      rr=sqrt(r*r+z*z)
c      hdens=getDPFrGridDens(r,Halo%DPFr)
      hdens=halodensity(rr)

      if(DiskFlag1) then
c        print*, "Disk 1",D1%DPFr%nr
        dsk1=getDPFrGridDens(r,D1%DPFr)        !1st Disk (see Disk/diskdensity.f)
      else
        dsk1=0.
      endif
      if(DiskFlag2) then
        dsk2=getDPFrGridDens(r,D2%DPFr)       !2nd Disk (see Disk/diskdensity.f)
      else
        dsk2=0.
      endif
      if(bulgeflag) then
        bdens = sersicdens(rr,Bulge)        !Bulge (see Bulge/sersicprofiles.f)
      else
        bdens = 0.
      endif
      if(GasFlag) then
        gdens=getDPFrGridDens(r,Gas%DPFr)
      else
        gdens=0.
      endif


      TotDens_Grid=dsk1+dsk2+hdens+bdens+gdens
c      print*, "New Dens Test", r,z,dsk1,dsk2,hdens,bdens,gdens

      return
      end function
ccccccccccc

ccccccc
      real function getDPFrGridDens(r,Grid)
      implicit none
      real r, r1,r2
      real t,tm1
      integer ihi
      Type(DPFrObj) Grid

      ihi=int(r/Grid%dr)+1
      if (ihi.le.1) ihi=1
      if (ihi.gt.Grid%nr) ihi=Grid%nr

c      print*, "Grid Dens Ini", r, ihi,Grid%nr

      r1=Grid%dr*(real(ihi)-1)
      r2=Grid%dr*real(ihi)
      t=(r-r1)/(r2-r1)
      tm1 = 1.0 - t
      getDPFrGridDens = (t*Grid%dens(ihi)+ tm1*Grid%dens(ihi-1))
c      print*, "Grid Dens Test", r,ihi,t,Grid%dens(ihi)
c     &          ,tm1,Grid%dens(ihi-1),getDPFrGridDens

      return
      end function
cccccc

      end module
