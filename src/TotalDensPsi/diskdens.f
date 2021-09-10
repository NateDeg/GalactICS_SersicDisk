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
      module DiskDensGridMod
      use DiskObjDef
      use CommonConsts
      use Globs
      use CalcPotGridMod
      use DiskDensMod

      implicit none
      contains
c
      real function diskdens(r,z,psi,D)

      real r,z,psi
      Type(DiskObj) D
      real con,coeff
      real psizh,psi00,dpsi,dpsizh
      real f,f1r,f2

c      print*, "Disk Dens Flag Check", BulgeFlag,HaloFlag
c      print*, "DiskDens Ini", r,z,psi,D%DiskUseFlag

      if(D%DiskUseFlag .eqv. .False.) then
        diskdens=0.
        return
      endif

c the truncation factor is derived assuming that the azimuthal velocity dist
c is gaussian with dispersion sigma_R * 2 omega/kappa, as per epicycle theory.


      if( abs(z/D%zdisk) .gt. 30.) goto 99
      if (z.eq.0.) then
        con=1
      elseif ( HaloFlag .eqv. .False.) then
        if( BulgeFlag .eqv. .False.) then
c only doing a disk potential
            con = exp(-abs(z/D%zdisk))
            con = (2.0*con/(1.0 + con*con))**2
c        print*, "only Disk Pot", con,BulgeFlag,HaloFlag
        else
            con=1.
        endif
      else
c     make disk vertically isothermal; normalize density so that
c     at height 3*zdisk the vertical density drop is that of a sech^2.
        psizh=pot(r,3*D%zdisk)
        psi00=pot(r,0.)
        dpsizh = psi00 - psizh
        dpsi   = psi00 - psi
        coeff = 0.0
        if( abs(dpsizh) .gt. 0.0) then
            coeff = dpsi/dpsizh
        endif
        if( coeff .gt. 16. .or. coeff .lt. 0.0 ) then
            con = 0.0
        else
            con = 0.009866**coeff
        endif
      endif

      call disksurfdens(r,f,f1r,f2,D)
c      diskdens = 0.5/zdisk*f*con*trunfac
      diskdens = 0.5/D%zdisk*f*con
c      print*, "DiskDens", r,z,psi!,f,con,coeff,psi00,psizh

      return
99    diskdens=0.0
      return
      end function



      end module
