cccccccccc
c
c     Halo Potential
c
c       This module contains routines for calculated the halo
c       density-potential pair
c
ccccccccc

      module BulgePotMod
      use Globs
      use BulgeGridDensMod
      use SimpsonIntMod

      implicit none
      contains
ccccc

      subroutine bulgepotential(bulgemass, bulgeedge,lmax)
      implicit none

      real bulgemass,bulgeedge
      integer lmax,ntheta
      real frac
      integer lmaxold
      integer ir
      real r
c
      print*, "Bulge Dens-Potential Calc"

      frac=0.75
      ntheta=lmax*4+2
      lmaxold=-2
      call GetBulgeDens(lmax,ntheta)
      call SimpsonHarmInt_HighL(nr,lmax,dr,lmaxold,frac
     &          ,BulgePot%Dens,BulgePot%Pot,BulgePot%Fr,BulgePot%Fr2
     &          ,BulgePot%lmax)

      bulgemass = BulgePot%fr(1,nr)/sqrt(4*pi)*(real(nr)*dr)**2
      do ir=1, nr
        r = real(ir)*dr
        if( BulgePot%dens(1,ir) .eq. 0.0 ) then
            bulgeedge = real(ir)*dr
            goto 999
        endif
      enddo
      bulgeedge = real(nr)*dr
999   write(*,*) 'Bulge mass =', bulgemass
      write(*,*) 'Bulge edge radius =', bulgeedge


      return
      end subroutine
ccccccccc





      end module
