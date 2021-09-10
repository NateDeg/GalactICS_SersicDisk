ccccccccccc
c
c     Generate density-psi table
c
c       This module contains routines that calculate a table of energies and
c       associated densities
c
cccccccccc

      module DensFromPsiMod
      use Globs
      use CalcPotGridMod
      use DiskDensGridMod
      use GasDensPsiMod
      use HaloDensPsiMod
      use BulgeDensPsiMod

      implicit none


      contains
cccc
      real function totdens_FrPsi(r,z)
      implicit none
      real psi
      real totdens,ddns,ddns2,gdns,hdns,bdns
      real r,z

      totdens = 0
      ddns = 0.
      ddns2 = 0.
      gdns = 0.
      hdns = 0.
      bdns = 0.

      psi=pot(r,z)

      if(DiskFlag1) then
        ddns = diskdens(r,z,psi,D1)
        totdens = totdens + ddns
      endif
      if( Diskflag2 ) then
        ddns2 = diskdens(r,z,psi,D2)
        totdens = totdens + ddns2
      endif
      if( GasFlag  ) then
        gdns = gasdenspsi(r,z,psi,Gas)
        totdens = totdens + gdns
      endif
      if(HaloFlag) then
        hdns = halodenspsi(psi)
        totdens = totdens + hdns
      endif
      if( BulgeFlag) then
        bdns = bulgedenspsi(psi)
        totdens = totdens + bdns
      endif
c      print*, "Tot Dens", r,z,psi,totdens,ddns,ddns2,gdns,bdns,hdns

      if(totdens .lt. 0.) totdens=0.

      if(isnan(totdens) .or. totdens .lt. 0.) then
        write(*,*) 'inside totdens_FrPsi',totdens,ddns,ddns2,
     +        gdns,hdns,bdns,psi
        write(*,*) psi
        stop
      endif
      totdens_FrPsi=totdens

      return
      end function
ccccccccc

      end module
