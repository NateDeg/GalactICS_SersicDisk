ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module CalcFreqsMod
      use Globs
      use GenCompGlobs
      use CalcPotGridMod
      use ForceFromGridMod

      implicit none

      contains

ccccccc
      subroutine CalcFreqs()
      implicit none
      integer ir

      print*, "Calculating Frequencies"
      call CalcPotOnAxis(TotPot)
c      print*, "Done Pot on Axis"
      call CalcVertFreqs(TotPot)
c      print*, "Done Vertical Freqs"
      call CalcPsi2(TotPot)
c      print*, "Done Psi2"

      if(OriBulgeFlag) then
        call CalcPotOnAxis(BulgePot)
        call CalcVertFreqs(BulgePot)
      else
        do ir=0,nr
            BulgePot%vcmaj(ir)=0.
        enddo
      endif

      call CalcPotOnAxis(HaloPot)
      call CalcVertFreqs(HaloPot)



      return
      end subroutine
ccccccccc

ccccccc
      subroutine CalcPotOnAxis(P)
      implicit none
      integer ir
      Type(PotObj) P
      real r

      do ir=0,nr
        r=real(ir)*dr
c        print*, "On Axis",ir,r
        P%potmaj(ir)=pot(r,0.)
        P%potup(ir)=pot(r*cos(0.05),r*sin(0.05))
      enddo

      return
      end subroutine
cccccccc

ccccccc
      subroutine CalcVertFreqs(P)
      implicit none
      integer ir
      Type(PotObj) P
      real r,z
      real fr,fz

      do ir=0,nr
        r=real(ir)*dr
        z=0.
        if(ir .eq. 0) then
            call ForceFromGrid(r,z,fr,fz,P)
            P%vcmaj(ir)=0
            P%vertfreq(ir)=0
        else
            call ForceFromGrid(r,z,fr,fz,P)
            P%vcmaj(ir)=sqrt(max(0.,-r*fr))
            P%vertfreq(ir)=sqrt(max(0.,P%vcmaj(ir)**2+
     &              2.*(P%potup(ir)-P%potmaj(ir))/0.05**2))/r
        endif
      enddo

      return
      end subroutine
cccccccc

ccccccc
      subroutine CalcPsi2(P)
      implicit none
      integer ir
      Type(PotObj) P
      real r

      do ir=0,nr
        r=real(ir)*dr
        P%Psi2(ir)=FR2FromGrid(r,0.,P)
      enddo

      return
      end subroutine
cccccccc


      end module
