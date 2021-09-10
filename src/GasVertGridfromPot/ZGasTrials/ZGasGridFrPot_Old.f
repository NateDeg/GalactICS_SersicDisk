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
      integer ir
      real r,radj,rmax,rarg
      real tdns
      real f,f1r,f2
      real zgasofrmax,zgasofr
      real errfac
      real,ALLOCATABLE :: zGasGridTemp(:),RGasGridTemp(:)

c      print*, "Get grid of heights for gas New"
      ALLOCATE(zgasGridTemp(0:Gas%DPFr%nr))
      ALLOCATE(RgasGridTemp(0:Gas%DPFr%nr))

      do ir = 0,Gas%DPFr%nr
        r = real(ir)*Gas%DPFr%dr
        if(r.eq.0.) then
            radj = eps
        else
            radj = r
        endif

        if(Gas%gamma.lt.0.) then
            rmax = Gas%outgas + 4.*Gas%drtruncgas
c            tdns=TotDens_NoGas(rmax,0.)+gasdensestimate(rmax,0.,Gas)
            tdns=TotDens_Grid(rmax,0.)
            tdns=4.*Pi*tdns
            call gassurfdens(rmax,f,f1r,f2,Gas)     !Gas surface density (Gas/gasdensity.f)
            f = 2.*pi*f
            zgasofrmax = (-f/4. + (f*f/16. + Gas%zgas0*tdns)**.5)/tdns  !Truncation Terms

            rarg = (radj-rmax)/Gas%drtruncgas
            if(rarg.gt.4.) then
                errfac = 1.
                zgasofr = 0.
            else
                if(rarg.lt.-4.) then
                    errfac = 0.
                else
                    errfac = 0.5*(erf(rarg) + 1)
                endif
c                tdns=TotDens_NoGas(radj,0.)+gasdensestimate(radj,0.,Gas)
                tdns=TotDens_Grid(radj,0.)
                tdns=4.*Pi*tdns
                call gassurfdens(Gas%outgas,f,f1r,f2,Gas)
                f = 2.*pi*f
            endif
            zgasofr = (-f/4. + (f*f/16. + Gas%zgas0*tdns)**.5)/tdns
        endif
        zgasgridTemp(ir) = (1.-errfac)*zgasofr + errfac*zgasofrmax
        rgasgridTemp(ir) = r
c        print*, "hmmm", ir,zgasofr,zgasofrmax,zgasgridTemp(ir)
        if(zgasgridTemp(ir).lt.1e-20.or.zgasgridTemp(ir).
     &          eq.zgasgridTemp(ir)+1..or.isnan(zgasgridTemp(ir)))
     &              then
            write(*,*) 'bad getzgas'
            write(*,*) 'getzgas,r',Gas%zgasgrid(ir),r,zgasgridTemp(ir)
            stop
        endif
c      print*, "New ZGrid test",ir,rarg,zgasgridTemp(ir)
c     &              ,rgasgridTemp(ir),zgasofr,zgasofrmax
      enddo
      do ir=0,Gas%DPFr%nr
c        print*, "New Grid Test",ir,real(ir)*Gas%DPFr%dr
c     &                  ,Gas%zgasgrid(ir),zgasgridTemp(ir)
        Gas%zgasgrid(ir)=zgasgridTemp(ir)
        Gas%rgasgrid(ir)=rgasgridTemp(ir)
      enddo
      DEALLOCATE(zgasgridTemp)
      DEALLOCATE(rgasgridTemp)

      return
      end subroutine
ccccccc



      end module
