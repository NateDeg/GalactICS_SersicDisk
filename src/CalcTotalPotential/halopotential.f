cccccccccc
c
c     Halo Potential
c
c       This module contains routines for calculated the halo
c       density-potential pair
c
ccccccccc

      module HaloPotMod
      use Globs
      use HaloGridDensMod
      use SimpsonIntMod

      implicit none
      contains
ccccc

      subroutine halopotential(halomass, haloedge,lmax)
      implicit none

      real halomass,haloedge
      integer lmax,ntheta
      real frac
      integer lmaxold
      integer ir
      real r
c
      print*, "Halo Dens-Potential Calc"

      frac=0.75
      ntheta=lmax*4+2
      lmaxold=-2
      call GetHaloDens(lmax,ntheta)
      call SimpsonHarmInt_HighL(nr,lmax,dr,lmaxold,frac
     &          ,HaloPot%Dens,HaloPot%Pot,HaloPot%Fr,HaloPot%Fr2
     &          ,HaloPot%lmax)

      halomass = HaloPot%fr(1,nr)/sqrt(4*pi)*(real(nr)*dr)**2
      do ir=1, nr
        r = ir*dr
        if( HaloPot%dens(1,ir) .eq. 0.0 ) then
            haloedge = real(ir)*dr
            goto 999
        endif
      enddo
      haloedge = real(nr)*dr
999   write(*,*) 'Halo mass =', halomass
      write(*,*) 'Halo edge radius =', haloedge


      return
      end subroutine
ccccccccc





      end module
