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
      module DiskDensMod
      use DiskObjDef
      use CommonConsts

      implicit none
      contains
c
      real function diskdensestimate(s,z,D)
      implicit none
      real s,z,r
      Type(DiskObj) D
      real f,f1r,f2
      real g,g1r,g2
c
      r=sqrt(s*s+z*z)

      call disksurfdens(r,f,f1r,f2,D)
      call diskvertdens(z,g,g1r,g2,D)

      diskdensestimate = 0.5*f*g2/D%zdisk
c      print*, "Disk Dens Estimate routine", s,z,f,g2,D%zdisk

      return
      end function
cccccccccccccc



cccccccccccccc
      subroutine diskvertdens(z,g,g1,g2,D)
      implicit none
      real z, zz, g,g1,g2
      Type(DiskObj) D

      zz = z/D%zdisk
      if(abs(zz).gt.50.) then
        g = abs(zz)
        g1 = sign(1.0,zz)
        g2 = 0.
      else
        g = log(cosh(zz))
        g1 = tanh(zz)
        g2 = 1./cosh(zz)**2.
      endif
c      print*, "disk vert dens", z, zz, g, g1, g2

      return
      end subroutine
ccccccccccc

cccccc
      subroutine disksurfdens(r,f,f1r,f2,D)
      implicit none
      real r, f, f1r,f2
      Type(DiskObj) D
      real t, t2,tmp2
      real eexp,eerfc
      real arg1,arg2,arg3
      real sg,sg1,sg2


      t=sqrt(0.5e0)*(r-D%outdisk)/D%drtrunc
      t2=t*t
      if (t.lt.-4.0) then
        eexp=0.
        eerfc=1.
      elseif (t.lt.4.0) then
        eexp=exp(-t2)/sqrt(2*pi)/D%drtrunc
        eerfc=0.5*erfc(t)
      else
        eexp=0
        eerfc=0
      endif

      arg1 = -(r/D%rdisk)**(1./D%ndisk)
      arg3=(r/D%rdisk)

      if(D%rhole.eq.0.) then
        sg = D%diskconst*exp(arg1)
c        sg1 = -D%diskconst/D%rdisk*exp(arg1)
c        sg2 = D%diskconst/(D%rdisk**2)*exp(arg1)
        sg1=-(sg/(D%ndisk*D%rdisk))*(arg3**(1./D%ndisk-1.))

        sg2 = -sg1/(D%ndisk*D%rdisk)*(arg3**(1./D%ndisk-1.))
     &          -sg/(D%rdisk**2.)*(1./D%ndisk-1.)*(1./D%ndisk)
     &          *(arg3**(1./D%ndisk-2))
      else
        tmp2 = sqrt(r**2. + D%rhole**2.)
        arg2 = -tmp2/D%rcore
        sg = D%diskconst*(exp(arg1) - exp(arg2))
        sg1 = D%diskconst*(-1./D%rdisk*exp(arg1) +
     &          r/D%rcore/tmp2*exp(arg2))
        sg2 = D%diskconst*((1./D%rdisk**2.)*exp(arg1) + exp(arg2)*
     &          (D%rcore*D%rhole**2. - r**2.*tmp2)/D%rcore**2./tmp2**3.)
      endif

      f = sg*eerfc

      if (r.gt.0.) then
        f1r = (sg1*eerfc + eexp*sg)/r
        f2 = sg2*eerfc + 2.*sg1*eexp +
     &      eexp*((r-D%outdisk)/D%drtrunc**2.)*sg
      else
        f1r = 0
        f2 = 0
      endif

c      print*, "disk surf dens", r,f,f1r,f2,sg,sg1,sg2

      if(isnan(f).or.f.gt.1e10) then
        write(*,*) 'inside disksurf fac1,eerfc,r,rdisk,diskconst',
     &      f,eerfc,r,D%rdisk,D%diskconst
        stop
      endif

      return
      end subroutine
cccccc


cccccc
      real function dpolardiskdens(r,ctheta,D)
      implicit none
      real r, ctheta,z,s
      Type(DiskObj) D
      z=r*ctheta
      s=r*sqrt(1.0 - ctheta*ctheta)
      dpolardiskdens=diskdensestimate(s,z,D)
c      print*, "Polar Dens Test", r, ctheta,z,s,dpolardiskdens
      return
      end function
cccccccc


      end module
