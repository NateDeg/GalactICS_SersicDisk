cccccccccccccccccc
c
c      Gas Z Calculations
c
c       This module contains routines that calculate
c       a grid of vertical heights for the grid
c
cccccccccccccccc

      module GasZMod
      use Globs
      use TotDensMod
      use GasSurfDensMod

      implicit none


      contains
ccccccccc
      subroutine getzgasgrid_old()
      implicit none
      integer ir
      real r,radj,rmax,rarg
      real tdns
      real f,f1r,f2
      real zgasofrmax,zgasofr
      real errfac
      real TTest

      print*, "Get grid of heights for gas"

      do ir = 0,Gas%DPFr%nr
        r = real(ir)*Gas%DPFr%dr
        if(r.eq.0.) then
            radj = eps
        else
            radj = r
        endif

        if(Gas%gamma.lt.0.) then
            rmax = Gas%outgas + 4.*Gas%drtruncgas
            tdns=TotDens_NoGas(rmax,0.)
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
                tdns=TotDens_NoGas(radj,0.)
                tdns=4.*Pi*tdns
                call gassurfdens(Gas%outgas,f,f1r,f2,Gas)
                f = 2.*pi*f
            endif
            zgasofr = (-f/4. + (f*f/16. + Gas%zgas0*tdns)**.5)/tdns
        endif
        Gas%zgasgrid(ir) = (1.-errfac)*zgasofr + errfac*zgasofrmax
        Gas%rgasgrid(ir) = r
        TTest=Gas%zgasgrid(ir)
        if(Gas%zgasgrid(ir).lt.1e-20.or.Gas%zgasgrid(ir).
     &          eq.Gas%zgasgrid(ir)+1.
     &          .or.TTest .eq. TTest+1)then
            write(*,*) 'bad getzgas'
            write(*,*) 'getzgas,r',Gas%zgasgrid(ir),r
            stop
      endif
c      print*, "No Gas ZGrid test",ir,rarg,Gas%zgasgrid(ir)
c     &              ,Gas%rgasgrid(ir)
      enddo

      return
      end subroutine
ccccccc



ccccccc
      real function getzgas(r,G)
      implicit none
      real r,del
      integer ir
      Type(GasObj) G

      ir = int(r/dr)
      if(ir .eq. nr) ir=ir-1
      del = r/G%DPFr%dr - float(ir)
      getzgas = G%zgasgrid(ir)
     &          + del*(G%zgasgrid(ir+1)-G%zgasgrid(ir))

c      if(getzgas .lt. 0.) then
c        print*, "Negative gas height"
c        print*, r,dr,ir,del,getzgas
c     &              ,G%zgasgrid(ir+1),G%zgasgrid(ir)
c        stop
c      endif
c      print*, "getz", ir,r,del,getzgas,G%zgasgrid(ir)

      return
      end function
cccccccccc


ccccccccc
      subroutine gasscaleheight(r,h,h1,h2,G)
      implicit none
      real r,h,h1,h2
      Type(GasObj) G
      real deltar,hm,hp

c      print*, "in gasscaleheight", r
      h = getzgas(r,G)
c      h=1.
      if(r.eq.0.) then
        h1 = 0.
        h2 = 0.
        return
      endif
      deltar = min(G%DPFr%dr,r/10.)
      hm = getzgas(r-deltar/2.,G)
      hp = getzgas(r+deltar/2.,G)
      h1 = (hp - hm)/deltar
      h2 = 4.*(hp - 2.*h + hm)/deltar/deltar
c      if(abs(h2) .gt. 72.) then
c        print*, "h2 value is large"
c        print*, r,deltar,h1,h2,h,hm,hp
c        stop
c      endif


c      print*, "Gasscaleheight", r,h,deltar,h1,h2

      if(h1.eq.h1+1) then
        write(*,*) r,dr,deltar,hm,hp
        stop
      endif

      return
      end subroutine
ccccccccccc


      end module
