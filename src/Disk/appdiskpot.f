cccccccccc
c
c       Approximate Disk Potential
c
c       This module calculates the approximate disk potential
c
ccccccccc
      module AppDiskPotMod
      use DiskObjDef
      use DiskDensMod
      use CommonConsts
      implicit none
      contains
cccccc
ccccccc
      real function appdiskpot(s,z,D)
      implicit none
      real s,z,r
      Type(DiskObj) D
      real f,f1r,f2
      real g,g1,g2

      r=sqrt(s*s+z*z)

      call disksurfdens(r,f,f1r,f2,D)
      if(f.eq.0.) then
         appdiskpot = 0.
         return
      else
         call diskvertdens(z,g,g1,g2,D)
         appdiskpot = -4*pi*f*D%zdisk*g
      endif

      appdiskpot = appdiskpot/2.

      return
      end function
ccccccc

ccccccc
      subroutine appdiskforce(s,z,fsad,fzad,D)
      implicit none
      Type(DiskObj) D
      real s,z,r,h,fsad,fzad
      real f,f1r,f2
      real g,g1,g2

      r=sqrt(s*s + z*z)

      call disksurfdens(r,f,f1r,f2,D)
      call diskvertdens(z,g,g1,g2,D)

      h = D%zdisk

      fsad = -2.0*pi*s*f1r*h*g
      fzad = -2.0*pi*(z*f1r*h*g + f*g1)

c      print*, "Approx disk force", s,z,fsad,fzad
c     &          ,f1r,h,g,g1

      return
      end subroutine
cccccccc


ccccc
      real function appdiskdens(s,z,D)
      implicit none
      Type(DiskObj) D
      real s,z,r,h
      real f,f1r,f2
      real g,g1,g2
c  This is the density corresponding to the first-guess disk potential
c  f(r)*erfc((r-outdisk)/sqrt(2)drtrunc)/2 * 4 pi G zdisk**2 log(z/zdisk)
c  where r is spherical radius. f(r) is here taken as an exponential.
c
c  The corresponding density is:
c  f(r)*erfc*sech(z/zdisk)**2 + radial gradient terms.
c  For radii below one scale radius, we have replaced the radial exponential
c  (which gives rise to a singular laplacian) with a quartic that joins on
c  smoothly.
c
      r=sqrt(s*s+z*z)
      call disksurfdens(r,f,f1r,f2,D)
      call diskvertdens(z,g,g1,g2,D)
      h = D%zdisk
      if(s.gt.0.) then
        appdiskdens =   f2*h*g +
     &                  2*f1r*g*h +
     &                  2.*f1r*g1*z +
     &                  f*g2/h
      else
        appdiskdens =   f2*h*g +
     &                  2*f1r*g*h +
     &                  2.*f1r*g1*z +
     &                  f*g2/h
      endif
      appdiskdens = appdiskdens/2.
      return
      end function
ccccccc


      end module
