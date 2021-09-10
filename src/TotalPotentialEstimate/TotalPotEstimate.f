cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module estimates the potential for the various
c       components of the galaxy
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module TotPotEstMod
      use Globs
      use HaloPotEstMod
      use DiskPotEstMod
      use GasZMod
      use GasPotEstMod

      implicit none

      contains

      subroutine TotPotEst()
      implicit none

      print*, "Estimating potential"
      if(HaloFlag) call halopotentialestimate(Halo)
      if(diskflag1) call diskpotentialestimate(D1)
      if(diskflag2) call diskpotentialestimate(D2)
      if(GasFlag) then
        call getzgasgrid_old()
        call gaspotentialestimate()
      endif

      return
      end subroutine


      end module

