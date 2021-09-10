ccccccccccc
c
c       Gas Potential Estimation Module
c
c     This module contains routines to estimate the initial potential of the Gas
ccccccccccccc
      module GasPotEstMod
      use Globs
      use SimpsonIntMod
      use GasZMod
      use GasDensMod
      implicit none

      contains
cccccccc
      subroutine gaspotentialestimate()
      implicit none

      integer ntheta
      integer ir,is
      real r
      real cthetamax,dctheta,ctheta
      real s
      real zgas

      print*, "Gas Pot Estimate"

      ntheta=100
      s=0.
      do ir=0,Gas%DPFr%nr
        r=ir*Gas%DPFr%dr
        zgas = getzgas(r,Gas)
        cthetamax = min(1.,10.*zgas/r)
        dctheta=cthetamax/float(ntheta)
        s=s+dpolargasdens(r,cthetamax,Gas)+dpolargasdens(r,0.0,Gas)
        do is=1,ntheta-1,2
            ctheta=is*dctheta
            s=s+4*dpolargasdens(r,ctheta,Gas)
        enddo
        do is=2,ntheta-2,2
            ctheta=is*dctheta
            s=s+2*dpolargasdens(r,ctheta,Gas)
        enddo
        s=s*dctheta/3.
        Gas%DPFr%dens(ir)=s
c        print*, "Gas Pot Test", ir, Gas%dens(ir)
      enddo

c     now get the potential harmonics of this new density. (BT 2-208)
c     Simpson's rule integration.
      call SimpsonHarmInt(Gas%DPFr%nr,Gas%DPFr%dr,Gas%DPFr%dens
     &                      ,Gas%DPFr%pot,Gas%DPFr%fr)
c
c      do ir=0,Gas%nr
c        print*, "Test", ir,Gas%dens(ir),Gas%pot(ir),Gas%fr(ir)
c      enddo

      open(file='gasdensityestimate.out',unit=29,status='replace')
      do ir = 1,nr
        r = ir*dr
        write(29,*) r,Gas%DPFr%pot(ir),Gas%DPFr%fr(ir)
     &                  ,Gas%DPFr%dens(ir),getzgas(r,Gas)
      enddo
      close(29)

      return
      end subroutine

      end module

