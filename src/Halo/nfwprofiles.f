ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module NFWMod
      use Globs
      implicit none

      contains

cccccccc
      subroutine nfwdens(R, dens)
      implicit none
      real,Intent(IN) :: R
      real,intent(out) :: dens
      real s
c      print*, "nfw density"
      s = R/Halo%a
      dens = Halo%haloconst/(s**Halo%cusp)/((1.+s)**(3.-Halo%cusp))
c      print*, "nfw dens", R, s,
c     &          Halo%haloconst/(s**Halo%cusp)/((1.+s)**(3.-Halo%cusp))
c     &          ,Halo%a
      return
      end subroutine
ccccccccccccccc

ccccccccc
      subroutine nfwdensprime(R,dens1)
      implicit none
      real s,dens
      real,Intent(IN) :: R
      real,intent(out) :: dens1

      s = R/Halo%a
      call nfwdens(R,dens)
      dens1 = -dens/Halo%a*(3.*s+Halo%cusp)/s/(1.+s)
      return
      end subroutine
ccccccccccc

cccccccc
      subroutine nfwdens2prime(R,dens2)
      implicit none
      real s,dens
      real,Intent(IN) :: R
      real,intent(out) :: dens2
      s = R/Halo%a
      call nfwdens(R,dens)
      dens2 = dens/Halo%a/Halo%a*
     &     (Halo%cusp*(Halo%cusp+1.)+8.*Halo%cusp*s+12.*s*s)
     &      /s/s/(1.+s)/(1.+s)

      return
      end subroutine
ccccccccccc

ccccccccc
      real function eerfc(r)
      implicit none
      real r, t,t2
c      if(Halo%NumericalTableSwitch .eq. 1) then
c        eerfc=1.
c        return
c      endif

      t=sqrt(0.5)*(r-Halo%chalo)/Halo%drtrunchalo
c      t=sqrt(0.5)*(r-Halo%chalo-(2.*sqrt(0.5)
c     &          *Halo%drtrunchalo))/Halo%drtrunchalo
      t2=t*t
      if (t.lt.-4.) then
        eerfc=1.
      elseif (t.lt.4.) then
        eerfc=0.5*erfc(t)
      else
        eerfc=0
      endif
      return
      end function
cccccccccc

cccccccccc
      real function eerfcprime(r)
      implicit none
      real r, t, t2
c      if(Halo%NumericalTableSwitch .eq. 1) then
c        eerfcPrime=0.
c        return
c      endif


      t=sqrt(0.5)*(r-Halo%chalo)/Halo%drtrunchalo
c      t=sqrt(0.5)*(r-Halo%chalo-(2.*sqrt(0.5)
c     &          *Halo%drtrunchalo))/Halo%drtrunchalo
      t2=t*t
      if(t2.gt.16.) then
        eerfcprime = 0.
      else
        eerfcprime = -0.5*sqrt(2./pi)/Halo%drtrunchalo*exp(-t2)
      endif
      return
      end function
ccccccccc

cccccccccc
      real function eerfc2prime(r)
      implicit none
      real r,t,t2
c      if(Halo%NumericalTableSwitch .eq. 1) then
c        eerfc2Prime=0.
c        return
c      endif

      t=sqrt(0.5)*(r-Halo%chalo)/Halo%drtrunchalo
c      t=sqrt(0.5)*(r-Halo%chalo-(2..*sqrt(0.5)
c     &          *Halo%drtrunchalo))/Halo%drtrunchalo
      t2=t*t

      if(t2.gt.16.) then
        eerfc2prime = 0.
      else
        eerfc2prime = 1./sqrt(pi)/Halo%drtrunchalo
     &                  /Halo%drtrunchalo*t*exp(-t2)
      endif

      return
      end function
ccccccccc



      end module
