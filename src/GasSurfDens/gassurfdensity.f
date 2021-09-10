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
      module GasSurfDensMod
      use GasObjDef
      use CommonConsts

      implicit none
      contains
ccccccccccc
      subroutine gassurfdens(r,f,f1r,f2,G)
      implicit none
      Type(GasObj) G
      real r, f, f1r,f2
      real t,t2
      real eexp,eerfc
      real fac1

      t=sqrt(0.5e0)*(r-G%outgas)/G%drtruncgas
      t2=t*t
      if (t.lt.-4.0) then
        eexp=0.
        eerfc = 1.
      elseif (t.lt.4.0) then
        eexp=exp(-t2)/sqrt(2*pi)/G%drtruncgas
        eerfc=0.5*erfc(t)
      else
        eexp=0
        eerfc=0
      endif

      if (r.gt.0.) then
        fac1 = G%gasconst*exp(-r/G%rgas)
        f = fac1*eerfc
        f1r = -fac1*(eerfc/G%rgas+eexp)/r
        f2 = fac1*(eerfc/G%rgas/G%rgas+
     &          eexp*(2/G%rgas+(r-G%outgas)/G%drtruncgas**2))
      else
        fac1 = G%gasconst
        f = fac1*eerfc
        f1r = 0
        f2 = 0
      endif

      if(isnan(f).or.f.gt.1e10) then
        write(*,*) 'inside gassurf fac1,eerfc,r,rgas,gasconst',
     &      fac1,eerfc,r,G%rgas,G%gasconst
        stop
      endif

      return
      end subroutine
cccccccc


      end module
