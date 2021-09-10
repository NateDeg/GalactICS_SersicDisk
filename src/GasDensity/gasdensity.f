ccccccccccccc
c
c     Gas Density Module
c
c       This module contains routines for calculating the gas density
c       contains gassurfdens
c
c
cccccccccccc
c
      module GasDensMod
      use GasObjDef
      use CommonConsts
      use GasZMod

      implicit none
      contains

cccccccccc
      real function dpolargasdens(r,ctheta,G)
      implicit none
      real r,ctheta,s,z
      Type(GasObj) G
      z=r*ctheta
      s=r*sqrt(1.0 - ctheta*ctheta)
      dpolargasdens=gasdensestimate(s,z,G)
      return
      end function
cccccccccc

cccccccccc
      real function gasdensestimate(s,z,GT)
      implicit none
      real s,z,r
      real f,f1r,f2
      real g, g1,g2,zgas
      Type(GasObj) GT

c
      r=sqrt(s*s+z*z)

      call gassurfdens(r,f,f1r,f2,GT)
      zgas = getzgas(r,GT)
      call gasvertdens(z,zgas,g,g1,g2)

      gasdensestimate = 0.5*f*g2/zgas

      return
      end function
cccccccccccc

cccccccccc
      subroutine gasvertdens(z,zgas,g,g1,g2)
      implicit none
      real z, zgas, g,g1,g2
      real zz

      zz = z/zgas
      if(abs(zz).gt.50.) then
        g = abs(zz)
        g1 = 1.
        g2 = 0.
      else
        g = log(cosh(zz))
        g1 = tanh(zz)
        g2 = 1./cosh(zz)**2.
      endif

      return
      end subroutine
cccccccccccc


ccccccc
      real function appgasdens(s,z,GT)
      implicit none
      Type(GasObj) GT
      real s,z,r
      real f,f1r,f2
      real g,g1,g2
      real h,h1,h2
c      print*, "In appgasdens",s,z

      r=sqrt(s*s+z*z)

      call gassurfdens(r,f,f1r,f2,GT)
      call gasscaleheight(s,h,h1,h2,GT)
      call gasvertdens(z,h,g,g1,g2)

      if(s.gt.0.) then
        appgasdens =
     +        f2*h*g +
     +        2*f1r*g*(h + h1*s) +
     +        2.*f1r*g1*z*(1.-s*h1/h) +
     +        f*g2/h*(1. + (z*h1/h)**2.) -
     +        f*g1*z/h*(h2+ h1/s) +
     +        f*g*(h2 + h1/s)
      else
        appgasdens =
     +        f2*h*g +
     +        2*f1r*g*h +
     +        2.*f1r*g1*z*(1.-s*h1/h) +
     +        f*g2/h*(1. + (z*h1/h)**2.) -
     +        f*g1*z/h*h2 +
     +        f*g*h2
      endif

      appgasdens = appgasdens/2.
c      appgasdens=1.
c      print*, "done appgasdens",appgasdens

      if(isnan(appgasdens)) then
        write(0,*) 'nan for appgasdens r,z,f,f1r,f2',
     +        r,z,f,f1r,f2
        write(0,*) h,h1,h2
        write(0,*) g,g1,g2
        stop
      endif

      return
      end function
cccccccccc

      end module
