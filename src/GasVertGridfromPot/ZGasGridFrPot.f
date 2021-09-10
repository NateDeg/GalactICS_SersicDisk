cccccccccccccccccc
c
c      Gas Z Calculations
c
c       This module contains routines that calculate
c       a grid of vertical heights for the grid
c
cccccccccccccccc

      module GasZGridMod
      use Globs
      use TotDensMod
      use GasSurfDensMod
      use CalcPotGridMod
      use ForceFromGridMod

      implicit none


      contains
ccccccccc
      subroutine getzgasgrid()
      implicit none
      integer ir,i,imax
      real,ALLOCATABLE :: zGasGridTemp(:),RGasGridTemp(:)
      real pot1,pot2
      real alpha,fac,zn,rmax
      real fzn,frad,fvert
      real zgasofrmax,zgasofr
      real r,rarg
      real errfac
      real tol,znPrime,ZTolTest
      real SlowStep

      real zmin
      zmin=0.001
      imax=20
      tol=1.e-3

c      print*, "Get grid of heights for gas New"
      ALLOCATE(zgasGridTemp(0:Gas%DPFr%nr))
      ALLOCATE(RgasGridTemp(0:Gas%DPFr%nr))


      alpha=2.*log(cosh(1.))
      fac = 25.
      rmax = Gas%outgas + 4.*Gas%drtruncgas
      pot1 = pot(rmax,0.0)
      pot2 = pot(rmax,rmax/fac)
      zn = rmax/fac*sqrt(Gas%zgas0/(pot1-pot2))
c      print*, "Start New Grid", pot1,pot2,pot1-pot2
      do i=1,imax
        fzn = pot(rmax,0.0) - pot(rmax,zn) - alpha*Gas%zgas0
        call force(rmax,zn,frad,fvert,TotPot)
        znPrime = zn + fzn/fvert
        zTolTest=abs((znPrime-zn)/(real(znPrime+zn)/2.))
        zn=znPrime
c        print*, 'mas r in z grid',i,rmax,fzn,frad,fvert,zn,zTolTest
        if(zTolTest. le. Tol) goto 100
      enddo
100   zgasofrmax = zn


c      do ir = 0,1
      do ir = 0,Gas%DPFr%nr
        SlowStep=1.
        r = Gas%DPFr%dr*real(ir)
        if(r.eq.0.) then
            zn = Gas%DPFr%dr/fac*sqrt(Gas%zgas0
     &              /(pot(r,0.0)-pot(r,Gas%DPFr%dr/fac)))
        else
            zn = r/fac*sqrt(Gas%zgas0/(abs(pot(r,0.0)-pot(r,r/fac))))
        endif
        if(zn .le. 0.) print*, "Neg zn Early", ir,zn

        rarg = (r-Gas%outgas)/Gas%drtruncgas
        if(rarg.gt.4.) then
            errfac = 1.
            zgasofr = 0.
        else
            if(rarg.lt.-4.) then
                errfac = 0.
            else
                errfac = 0.5*(erf(rarg) + 1)
            endif
            do i=1,imax
                fzn = pot(r,0.0) - pot(r,zn) - alpha*Gas%zgas0
                call force(r,zn,frad,fvert,TotPot)
150             SlowStep=SlowStep*0.5
                znPrime = zn + SlowStep*fzn/fvert
                if(znPrime .lt. 0.) goto 150

                zTolTest=abs((znPrime-zn)/(real(znPrime+zn)/2.))

                if(zn .lt.0.) print*, "neg zn",i,zn
c                print*, r, i,zn, znPrime,zTolTest
                zn=znPrime
                if(zTolTest. le. Tol) goto 200
            enddo
c            if(zn .lt. zmin) zn=zmin
        endif
200     zgasofr = zn
c        print*, "Gas Grid",r,zn
        zgasgridTemp(ir) = (1.-errfac)*zgasofr + errfac*zgasofrmax
        rgasgridTemp(ir) = r
      enddo



      do ir=0,Gas%DPFr%nr
        Gas%zgasgrid(ir)=zgasgridTemp(ir)
        Gas%rgasgrid(ir)=rgasgridTemp(ir)
      enddo
      DEALLOCATE(zgasgridTemp)
      DEALLOCATE(rgasgridTemp)

c      print*, "Done New Gas Grid"
      return
      end subroutine
ccccccc



      end module
