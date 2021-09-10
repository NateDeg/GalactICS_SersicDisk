cccccccccc
c
c       Approximate Disk Potential
c
c       This module calculates the approximate disk potential
c
ccccccccc
      module AppGasPotMod
      use GasObjDef
      use GasDensMod
      use GasZMod
      use CommonConsts
      implicit none
      contains
cccccc
ccccccc
      real function appgaspot(s,z,GObj)
      implicit none
      real s,z,r
      Type(GasObj) GObj
      real f,f1r,f2,zgas
      real g,g1,g2

      r=sqrt(s*s+z*z)
      zgas = getzgas(s,GObj)

      call gassurfdens(r,f,f1r,f2,GObj)
      if(f.eq.0.) then
        appgaspot = 0.
        return
      else
        call gasvertdens(z,zgas,g,g1,g2)
        appgaspot = -4*pi*f*zgas*g
      endif
      appgaspot = appgaspot/2.
      if(isnan(appgaspot).or.appgaspot+1.eq.appgaspot) then
        write(*,*) 'inside appgaspot s,z,f,g,zgas',s,z,f,g,zgas
        stop
      endif
      return
      end function
ccccccc

ccccc
      subroutine appgasforce(s,z,fsad,fzad,GObj)
      implicit none
      real s,z,fsad,fzad
      Type(GasObj) GObj
      real r
      real f,f1r,f2
      real zgas,h1,h2
      real g,g1,g2
      r=sqrt(s*s + z*z)

      call gassurfdens(r,f,f1r,f2,Gobj)
      call gasscaleheight(s,zgas,h1,h2,GObj)
      call gasvertdens(z,zgas,g,g1,g2)

      fsad = -2.0*pi*(s*f1r*zgas*g + f*h1*g - z*h1/zgas*f*g1)
      fzad = -2.0*pi*(z*f1r*zgas*g + f*g1)

      return
      end subroutine
cccccccc




      end module
