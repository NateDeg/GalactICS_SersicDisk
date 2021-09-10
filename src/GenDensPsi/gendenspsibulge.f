ccccccccccc
c
c     Generate density-psi table
c
c       This module contains routines that calculate a table of energies and
c       associated densities
c
cccccccccc

      module BulgeDensPsiMod
      use Globs
c      use GetPsiMod
      use DFObjDef
      use BulgeDFMod
      use GenDensPsiMod
      implicit none


      contains
cccc
      subroutine gendenspsibulge()
      implicit none
      character(100) fname
      integer m

      m=100
      fname='denspsibulge.dat'
      call gendenspsi(fname,DF%denspsibulge(1:DF%npsi),dfbulge,m)

      return
      end subroutine
ccccccccc

cccccc
      real function bulgedenspsi(energy)
      implicit none
      real energy
      integer j
      real rj, frac

      if(energy.lt.DF%psic) then
        bulgedenspsi = 0.
        return
      endif
      if(energy.ge.DF%psi0) then
        bulgedenspsi = DF%denspsibulge(1)
        return
      endif

      rj = 1. + float(DF%npsi-1)*
     &      log((DF%psi0-energy)/(DF%psi0-DF%psid))
     &      /log((DF%psi0-DF%psic)/(DF%psi0-DF%psid))
      j = int(rj)
      if(j.lt.1) j = 1
      if(j.ge.npsi) j = npsi-1

      frac = rj - float(j)
      bulgedenspsi = DF%denspsibulge(j) +
     &          frac*(DF%denspsibulge(j+1)-DF%denspsibulge(j))
      return
      end function
ccccccc



      end module
