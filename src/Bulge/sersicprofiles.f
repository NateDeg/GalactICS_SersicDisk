cccccccccccc
c
c     Sersic Profiles
c
c     This module contains routines used to calculate the
c     Sersic profiles used in GalactICS.  It requires the routines found in
c     BulgeObject.f


      module SersicProfilesMod
      use BulgeObjDef
      use GammaFnRoutines
      use CommonConsts

      contains

cccccccc
      subroutine BulgeIni(B,BF)
      implicit none
      Type(BulgeObj) B
      logical BF
c
      if(B%ppp.lt.0) then
        B%ppp = 1. - 0.6097/B%nnn + 0.05463/(B%nnn*B%nnn)
      endif
      call setsersicparameters(B,BF)
      return
      end subroutine
cccccccc


cccccccccc
      subroutine setsersicparameters(B,BF)
      implicit none
      Type(BulgeObj) B
      real comnen
      real butt1,butt2,buttacc
      logical BF

c      print*, "Set sersic parameters", B%nnn
      if(BF.eqv..False.) return

      comnen = B%nnn
      B%Re = B%abulge
      if (B%nnn.gt.0.50) then
        butt1=0.6*(2.0*B%nnn)
      elseif (B%nnn.le.0.50) then
        butt1=1.0E-4
      endif
      butt2=1.20*(2.0*B%nnn)
      buttacc=1.0E-4

      B%butt=RTBISS(funcbutt,butt1,butt2,buttacc,comnen)

      B%Rho0 = (B%v0bulge**2.)/(4.*pi*B%Re*B%Re*B%nnn
     &          *B%butt**(B%nnn*(B%ppp-2.))*
     &      exp( gammln(B%nnn*(2.-B%ppp))))
      print*, "Bulge constants", B%nnn,B%Re,B%v0bulge
      return
      end subroutine

cccc

ccccccccc
      real function sersicpot(rad,B)
      implicit none
      Type(BulgeObj) B
      real L1,L2
      real rad,u,un,aaa

      if(rad.eq.0.) then
        sersicpot = B%v0bulge**2.
        return
      endif

      u = rad/B%Re
      un = u**(1./B%nnn)
      aaa = B%nnn*(3.-B%ppp)
      L1 = B%Rho0*B%Re*B%Re*B%nnn*B%butt**(B%nnn*(B%ppp-2.))*
     &     gammq(B%nnn*(2.-B%ppp),B%butt*un)
     &      *exp(gammln(B%nnn*(2.-B%ppp)))
      L2 = B%Rho0*(B%Re**3.)*B%nnn*(B%butt**(-aaa))*exp(gammln(aaa))
      if(aaa+1 .gt. B%butt*un) then
        L2 = L2*gammqm1(aaa,B%butt*un)
      else
        L2 = L2*(1.-gammq(aaa,B%butt*un))
      endif
      sersicpot=4.*pi*(L2/rad + L1)

      return
      END function
cccccccccc

ccccccc
      real function sersicmass(rad, B )
      implicit none
      Type(BulgeObj) B
      real rad,u,un,aaa,gm

      B%Re = B%abulge
      u = rad/B%Re
      un = u**(1./B%nnn)
      aaa = B%nnn*(3.-B%ppp)
      if(aaa+1 .gt. B%butt*un) then
        gm = gammqm1(aaa,B%butt*un)
      else
        gm = 1.-gammq(aaa,B%butt*un)
      endif
      sersicmass = 4.*pi*B%Rho0*(B%Re**3.)*B%nnn
     &          *(B%butt**(B%nnn*(B%ppp-3.)))*
     &          gm*exp(gammln(B%nnn*(3.-B%ppp)))

      return
      end function
cccccccccc

ccccccccc
      real function sersicdens(rad,B)
      implicit none
      Type(BulgeObj) B
      real rad, u,un

      u = rad/B%Re
      un = u**(1./B%nnn)
      sersicdens = B%Rho0*(u**(-B%ppp))*exp(-B%butt*un)

      return
      end function
ccccccccc

      real function sersicdensprime(rad, B)
      implicit none
      type(BulgeObj) B
      real rad, u, un

      u = rad/B%Re
      un = u**(1./B%nnn)
      sersicdensprime = -sersicdens(rad,B)/B%Re
     &          *(B%ppp*B%nnn+B%butt*un)/B%nnn/u

      return
      end function
ccccccccc

cccccccc
      real function sersicdens2prime(rad,B)
      implicit none
      Type(BulgeObj) B
      real rad,u,un

      u = rad/B%Re
      un = u**(1./B%nnn)
      sersicdens2prime = sersicdens(rad,B)/B%Re/B%Re*(
     &     (B%ppp*B%nnn)**2.+B%ppp*B%nnn*B%nnn
     &      +2.*B%ppp*B%butt*B%nnn*un
     &      +B%butt*un*(B%nnn-1)+(B%butt*un)**2.)/((B%nnn*u)**2.)
      return
      end function
ccccccccc

ccccccc
      real function sersicforce(rad,B)
      implicit none
      Type(BulgeObj) B
      real L2, rad, u, un,aaa

      u = rad/B%Re
      un = u**(1./B%nnn)
      aaa = B%nnn*(3.-B%ppp)

      L2 = B%Rho0*(B%Re**3.)*B%nnn*(B%butt**(-aaa))
     &          *exp(gammln(aaa))
      if(aaa+1 .gt. B%butt*un) then
        L2 = L2*gammqm1(aaa,B%butt*un)
      else
        L2 = L2*(1.-gammq(aaa,B%butt*un))
      endif

      sersicforce = -4.*pi*(L2/rad/rad)
      return
      end function
ccccccccc



C     ------------------------------------------------------------------
      real function funcbutt(b,comnen)
      implicit none
      real b,abe,comnen
      abe=2.*comnen
      funcbutt=( gammp(abe,b)-0.5 )
      return
      end  function
cccccccccccccc

      real FUNCTION rtbiss(funcbutt,x1,x2,xacc,comnen)
      implicit none
      INTEGER JMAX
      real x1,x2,xacc,lick
      PARAMETER (JMAX=100)
      INTEGER j
      real dx,f,fmid,xmid
      real comnen
      real funcbutt
      external funcbutt

      lick=comnen

c      write(6,*) 'b: ',2.*lick,x2

      fmid=gammp(2.*lick,x2)-0.5
      f=gammp(2.*lick,x1)-0.5

C      write(6,*) 'c:  F-s: ',fmid,f

      if(f*fmid.ge.0.) then
        print*, 'root must be bracketed in rtbiss'
        stop
      endif
      if(f.lt.0.)then
        rtbiss=x1
        dx=x2-x1
      else
        rtbiss=x2
        dx=x1-x2
      endif
      do 11 j=1,JMAX
        dx=dx*.5
        xmid=rtbiss+dx
        fmid=gammp(2.*lick,xmid)-0.5
        if(fmid.le.0.)rtbiss=xmid
        if(abs(dx).lt.xacc .or. fmid.eq.0.) return
11     continue
      print*, 'too many bisections in rtbiss'
      END function





      end module
