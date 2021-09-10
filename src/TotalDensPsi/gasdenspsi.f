ccccccccccccc
c
c     Gas Density from Psi Module
c
c       This module contains routines for calculating the gas density
c       using the psi array calculated earlier
c
cccccccccccc
c
      module GasDensPsiMod
      use GasObjDef
      use CalcPotGridMod
      use GasDensMod
      use SplineMod

      implicit none
      contains
c
cccccc
      real function gasdenspsi(r,z,psi,G)
      real r,z,psi
      Type(GasObj) G
      real f,f1r,f2
      real psir0
      real gn
cc
      call gassurfdens(r,f,f1r,f2,G)
      call splintd(G%gasnormrad,G%gasnorm,G%gasnorm2,G%nrsplg,r,gn)

      psir0 = pot(r,0.0)
      gasdenspsi = f/gn*exp((psi-psir0)/G%zgas0)

c      if(gasdenspsi .lt. 0.) then
c        print*, "Negative Gas Dens", r,z,f,gn,psir0,psi,gasdenspsi
c      endif

      if(isnan(gasdenspsi)) then
        write(*,*) 'nan for gasdenspsi in diskdens r,z',r,z
        write(*,*) 'f,gn',f,gn
        write(*,*) 'delta pot',pot(r,z)-pot(r,0.)
        stop
      endif

      return
      end function
ccccccc
      end module
