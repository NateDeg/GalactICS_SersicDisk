cccccccc
c
c     Disk DF Interz
c
c       Contains routines used for integrating the disk df
ccccccccccc

      module diskdfezMod
      use CommonConsts
      use CalcPotGridMod
      use RCircMod
      use DiskSigmaMod
      use fnamiddenMod
      implicit none

      contains
ccccc

ccccccc
      real function diskdf5ez(vr,vt,vz,r,z,D)
      implicit none


      real psir0,psirz
      real vr,vt,vz,r,z
      real ep,am,ez
      Type(DiskObj) D

      if(isnan(vt) .or. isnan(r)) then
        print*, "Problem in diskdf5ez", vr,vt,vz,r,z
      endif

      psir0=pot(r,0.0)
      if (z.eq.0.) then 
         psirz=psir0
      else
         psirz=pot(r,z)
      endif
      ep=0.5*(vr*vr+vt*vt)-psir0
      am=r*vt
      ez=0.5*vz*vz-psirz+psir0
      
      diskdf5ez=diskdf3ez(ep,am,ez,D)
      if(diskdf5ez.lt.0.) diskdf5ez = 0.

c      if(diskdf5ez .le. 0) then
c        print*,"diskdf5ez 0", r,z,vr,vt,vz,ez,psirz,psir0
c      endif

      return
      end function
ccccccccc

cccccccc
      real function diskdf3ez(ep,am,ez,D)
      implicit none
      real ep,am,ez,ec
      Type(DiskObj) D
      real psi00
      real fomega,fkappa,rcir,fvert,sr2,sz2,vc

      psi00 = pot(0.0,0.0)
      rcir=rcirc(am,D)
      call omekap(rcir,fomega,fkappa)
      vc=rcir*fomega
c
      ec = - pot(rcir,0.0)+0.5*vc**2
c
      if (am.lt.0.) ec = - 2.0*psi00 - ec
      sr2=sigr2(rcir,D)
      sz2=sigz2(rcir,D)

      if (sz2.gt.0.) then
        fvert=fnamidden(rcir,D)*exp(-ez/sz2)/sqrt(2*pi*sz2)
        diskdf3ez=fomega/(pi*fkappa)/sr2*exp(-(ep - ec)/sr2)*fvert
c        diskdf3ez=fomega/(pi*fkappa)/sr2*exp(-abs(ep - ec)/sr2)*fvert
      else
        diskdf3ez=0
      endif

c      if(diskdf3ez .eq. 0) then
c        print*, "diskdf3ez is zero",rcir,ep, am, ez
c     &              ,sz2,fvert
c     &              ,fnamidden(rcir,D)
c     &              ,-ez/sz2,exp(-ez/sz2)
c      endif

      return
      end function
ccccccc

      end module
