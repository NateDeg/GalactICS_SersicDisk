ccccccccccc
c
c       Halo Potential Estimation Module
c
c     This module contains routines to estimate the initial potential of the halo
ccccccccccccc
      module HaloPotEstMod
      use HaloObjDef
      use HaloDensityMod
      use SimpsonIntMod
      implicit none

      contains
cccccccc
      subroutine halopotentialestimate(H)
      implicit none
      Type(HaloObj) H
      integer ir
      real r,Test

      integer nr
      real rmin, rmax,logdR

c      print*, "Testing", H%DPFr%dens(0:H%DPFr%nr)
c      print*, "Estimating halo potential",H%DPFr%nr
c      do ir=0,100


      do ir=0,H%DPFr%nr
c        print*, "hmmm", ir
        r=ir*H%DPFr%dr
        if(r.eq.0.) r = H%eps
c        print*, "Before calling halodensity",ir,r,H%eps
c        Test=halodensity(r)
c        print*, "Gotten a halo dens", ir, Test
         H%DPFr%dens(ir)=halodensity(r)
c         write(10,*) r, halodensity(r)*mscale
c     &          ,halodensprime(r)*mscale,halodens2prime(r)*mscale
c        print*, "Initial Halo density",ir,r,H%DPFr%dens(ir)
      enddo


      nr=1000
      rmin=log10(2.58768734E-04)
      rmax=log10(180.)
      logdR=(rmax-rmin)/(nr-1)
c      rmin=0.01
c      rmax=real(nr)*rmin
c      logdR=(rmax-rmin)/(nr)
c      print*, rmin,rmax,logdR
      open(10,file='InitialHaloDens.txt',status='replace')
      write(10,*) nr
      do ir=1,nr
        r=(ir-1)*logdR+rmin
        r=10.**r
c        r=(ir)*logdR
        write(10,*) r,halodensity(r)*mscale
     &              ,halodensprime(r)*mscale
     &              ,halodens2prime(r)*mscale
      enddo
      close(10)

c     now get the potential harmonics of this new density. (BT 2-208)
c     Simpson's rule integration.
      call SimpsonHarmInt(H%DPFr%nr,H%DPFr%dr
     &          ,H%DPFr%dens,H%DPFr%pot,H%DPFr%fr)

c      do ir=0,100
ccc      do ir=1,H%DPFr%nr
c        print*, "Halo Ini Test", ir,H%DPFr%dens(ir)
c     &          ,H%DPFr%pot(ir),H%DPFr%fr(ir)
c      enddo

      return
      end subroutine 

      end module

