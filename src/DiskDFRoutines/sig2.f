ccccccccc
c
c       Disk Sigma Module
c
c       This module calculates the disk random motion profiles
c
ccccccccc

      module DiskSigmaMod
      use DiskObjDef
      use CalcPotGridMod
      use SplineMod
      implicit none

      contains
c
cccccccc
      real function sigr2(r,D)
      implicit none
      real r
      Type(DiskObj) D

c      sigr2=D%sigr0*D%sigr0
c     &          *exp(-(r/D%disksr)**(1./D%sig_n))
      sigr2=D%sigr01**2.*exp(-(r/D%disksr1))
     &          +D%sigr02**2. *exp(-(r/D%disksr2))
c      print*, "Sigr2", r,D%disksr,D%sigr0,D%sig_n,sigr2


      return
      end function
ccccccc

ccccccccc
      real function sigz2(r,D)   !Note that this return assumes spline has been run on disk D
      real r, psizh,psi00
      Type(DiskObj) D
      real truesigz2,fcor

      psizh=pot(r,D%zdisk)
      psi00=pot(r,0.0)
cc
      truesigz2=(psizh-psi00)/log(0.419974)
cc

      call splintd(D%rr(0:D%nrspl),D%fszrat(0:D%nrspl),D%szrat2(0:D%nrspl)
     &              ,D%nrspl+1,r,fcor)
      sigz2=truesigz2*fcor
      if(sigz2.lt.0.) sigz2 = 1.e-10

      return
      end function
ccccccc

      end module
