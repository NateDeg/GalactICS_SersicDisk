ccccccccccc
c
c       Disk Potential Estimation Module
c
c     This module contains routines to estimate the initial potential of the halo
ccccccccccccc
      module DiskPotEstMod
      use DiskObjDef
      use DiskDensMod
      use SimpsonIntMod
      implicit none

      contains
cccccccc
      subroutine diskpotentialestimate(D)
      implicit none
      Type(DiskObj) D
      integer ntheta
      integer ir,is
      real r
      real cthetamax,dctheta,ctheta
      real s

      print*, "Disk Pot Estimate"

      ntheta=100
      s=0.
c      do ir=0,0
      do ir=0,D%DPFr%nr
        r=ir*D%DPFr%dr
        if(r.eq.0.) r = D%eps
        cthetamax = min(1.,10.*D%zdisk/r)
        dctheta=cthetamax/float(ntheta)
        s=s+dpolardiskdens(r,cthetamax,D)+dpolardiskdens(r,0.0,D)
        do is=1,ntheta-1,2
            ctheta=real(is)*dctheta
            s=s+4*dpolardiskdens(r,ctheta,D)
c            print*, "hmmm", is, r,ctheta,s
        enddo
        do is=2,ntheta-2,2
            ctheta=real(is)*dctheta
            s=s+2*dpolardiskdens(r,ctheta,D)
        enddo
        s=s*dctheta/3.
        D%DPFr%dens(ir)=s
c        print*, "DiskDensIni",ir,r,D%DPFr%dens(ir)
      enddo

c     now get the potential harmonics of this new density. (BT 2-208)
c     Simpson's rule integration.
      call SimpsonHarmInt(D%DPFr%nr,D%DPFr%dr
     &              ,D%DPFr%dens,D%DPFr%pot,D%DPFr%fr)

c      do ir=0,D%nr
c        print*, "Test", ir,D%DPFr%dens(ir),D%DPFr%pot(ir),D%DPFr%fr(ir)
c      enddo

      return
      end subroutine 

      end module

