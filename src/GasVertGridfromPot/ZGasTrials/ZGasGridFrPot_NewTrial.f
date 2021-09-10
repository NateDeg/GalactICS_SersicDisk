cccccccccccccccccc
c
c      Gas Z Calculations
c
c       This module contains routines that calculate
c       a grid of vertical heights for the grid
c
cccccccccccccccc

      module GasZGridMod_newattempt
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
      integer ir,i
      real,ALLOCATABLE :: zGasGridTemp(:),RGasGridTemp(:)
      real pot1,pot2
      real alpha,fac,zn,rmax
      real fzn,frad,fvert
      real zgasofrmax,zgasofr
      real r,rarg
      real errfac
      real f,f1r,f2,rho0,zmin

c      print*, "Get grid of heights for gas New"
      ALLOCATE(zgasGridTemp(0:Gas%DPFr%nr))
      ALLOCATE(RgasGridTemp(0:Gas%DPFr%nr))

      zmin=1.e-3
      alpha=2.*log(cosh(1.))
      fac = 25.
      rmax = Gas%outgas + 4.*Gas%drtruncgas
c      call gassurfdens(rmax,f,f1r,f2,Gas)
      f=Gas%gasconst*exp(-rmax/Gas%rgas)
      call GasRho0(rmax,rho0)
      zn=f/rho0
      zgasofrmax = zn
c      print*, "zMax Test", zgasofrmax

c      do ir=0,1
      do ir =Gas%DPFr%nr,0,-1
cc        print*, "Start of z grid loop",ir
        r = Gas%DPFr%dr*real(ir)
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
            f=Gas%gasconst*exp(-r/Gas%rgas)
            call GasRho0(r,rho0)
            zn=f/rho0
            if(zn .lt. zmin) zn=zgasgridTemp(ir+1)
            zgasofr = zn
            print*, "hmmm", r,f,rho0,zn
        endif
        zgasgridTemp(ir) = (1.-errfac)*zgasofr + errfac*zgasofrmax
        rgasgridTemp(ir) = r
      enddo



      do ir=0,Gas%DPFr%nr
c        print*, "New Grid Test",ir,real(ir)*Gas%DPFr%dr
c     &                  ,Gas%zgasgrid(ir),zgasgridTemp(ir)
        Gas%zgasgrid(ir)=zgasgridTemp(ir)
        Gas%rgasgrid(ir)=rgasgridTemp(ir)
      enddo
      DEALLOCATE(zgasgridTemp)
      DEALLOCATE(rgasgridTemp)

c      print*, "Done New Gas Grid"
      return
      end subroutine
ccccccc

cccccccccc
      subroutine GasRho0(r,rho0)
      implicit none
      real r, rho0
      real hcurr,zmax,zt
      integer i,imax
      real dz, sum,psi,psi0

      psi0=pot(r,0.)
      hcurr=getzgas(r,Gas)
      zmax=5.*hcurr
      imax=10.
      dz=zmax/real(imax)

      sum=0.
      do i=1,imax
        zt=real(i-0.5)*dz
        psi=pot(r,zt)
        sum=sum+exp((psi-psi0)/Gas%zgas0)*dz
c        sum=sum+exp((psi)/Gas%zgas0)*dz
        print*, "rho0 test",i,r,zt,psi,sum,Gas%zgas0
      enddo
      rho0=2.*sum

      return
      end subroutine
ccccccccc

      end module
