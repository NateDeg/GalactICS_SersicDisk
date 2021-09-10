cccccccc
c
c     Disk DF Interz
c
c       Contains routines used for integrating the disk df
ccccccccccc

      module diskdfInterzMod
      use CommonConsts
      use CalcPotGridMod
      use RCircMod
      use DiskSigmaMod
      use fnamiddenMod
      implicit none

      contains
ccccc

ccccccc
      real function diskdf5intez(vt,r,z,D)
      implicit none

      real psir0,psirz
      real vt,r,z
      real ep,am,ez
      Type(DiskObj) D

      real T

c      print*, "Start of diskdf5intez", vt,r,z

      psir0=pot(r,0.0)
      if (z.eq.0.) then 
         psirz=psir0
      else
         psirz=pot(r,z)
         endif
      ep=0.5*(vt*vt)-psir0
      am=r*vt
      ez=-psirz+psir0
c      print*, "Calc ep in df5", r,vt,psir0,ep

      T=diskdf3intez(ep,am,ez,D)
      diskdf5intez=T
c      diskdf5intez=diskdf3intez(ep,am,ez,D)
      if(diskdf5intez.lt.0.) diskdf5intez = 0.

c      if(z .eq. 0.) then
c        print*, "Diskdf5intez", vt,ep,T
c      endif
      return
      end function
ccccccccc

cccccccc
      real function diskdf3intez(ep,am,ez,D)
      implicit none
      real ep,am,ez
      real psi00
      real Rcir
      Type(DiskObj) D
      real vc,ec,sr2,sz2
      real fomega,fkappa
      real fvert,arg

      real ps
      real eps

      eps=1.e-2

      psi00 = pot(0.0,0.0)

      Rcir=rcirc(am,D)
      call omekap(rcir,fomega,fkappa)
      vc=rcir*fomega
      ps=pot(rcir,0.0)
      ec= -pot(rcir,0.0)+0.5*vc*vc

c      print*, "Calc ec in df3", am,ec
      if (am.lt.0.) ec= -2*psi00-ec
c      print*, "new ec",ec
      sr2=sigr2(rcir,D)
      sz2=sigz2(rcir,D)
      if (sz2.gt.0.) then
        fvert=fnamidden(rcir,D)*exp(-ez/sz2)
        arg = (ep-ec)/sr2
c        arg = abs(ep-ec)/sr2
c        if(abs(ep-ec) .lt. eps) arg=eps/sr2
        if(arg .lt. 0.) arg=-arg
        diskdf3intez=fomega/fkappa*sqrt(2/pi/sr2)*
     &          exp(-(arg))*fvert
c        print*, "Disk3f3intez",arg,fomega,fkappa,fvert,sr2
c     &              ,sqrt(2/pi/sr2),fomega/fkappa
c     &              ,sqrt(2/pi/sr2)*fomega/fkappa*fvert
c     &              ,exp(-(arg)),diskdf3intez
      else
        diskdf3intez=0
      endif

      return
      end function
cccccccccc



      end module
