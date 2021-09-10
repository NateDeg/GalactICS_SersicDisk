ccccccccccccc
c
c     Grid Density Module
c
c       This module contains routines for calculating the density using
c       the potential arrays and the polardens routines
c
c     NB that dens only gives the density without an approximate sech**2
c     component --- that part of the potential is not represented by the
c     harmonics.  The function dens(r,z) returns the density -
c     high-frequency cpt The full density is returned by
c
c
cccccccccccc
c
      module GenGridDensMod
      use LegendreRoutines
      use DensFromPsiMod
      use AppDiskPotMod
      use GasDensMod

      implicit none
      contains
c
ccccccc
      subroutine GenGetNewDensities(lmax,ntheta,P,polard)
      implicit none
      integer lmax,ntheta

      integer l, ir,nrmx,is
      real r,s,dctheta,ctheta

      Type(PotObj) P
      real polard
      external polard

cccc

c      print*, "GenGetNewDensities"
      do l=2,lmax,2
        P%Dens(l/2+1,0)=0
      enddo
      do ir=1,nr
        P%dens(1,ir)=0
      enddo
c     nrmx will mark the outermost radial bin with non-zero density.
      nrmx=nr
c      do l=0,0
      do l=0,lmax,2
c        print*, 'Gen Grid Dens', l, lmax
c     integrate density * spherical harmonic function over quadrant use
c     cos(theta) as independent variable.
c        do ir=1,1
        do ir=1,nrmx
            r=real(ir)*dr
            s=0.
            dctheta=1.0/real(ntheta)
            s=s+polard(r,1.0,l)+polard(r,0.0,l)
            do is=1,ntheta-1,2
                ctheta=real(is)*dctheta
                s=s+4*polard(r,ctheta,l)
c                    print*, "S round 1", l,ir,is,s
            enddo
            do is=2,ntheta-2,2
                ctheta=real(is)*dctheta
                s=s+2*polard(r,ctheta,l)
            enddo
            s=s*dctheta/3.
            s=s*4*pi
c            print*, "New Dens", ir,l,l/2+1,s,TotPot%dens(l/2+1,ir)
            P%dens(l/2+1,ir)=s

c     mark the first even radial bin on which the density has fallen to
c     zero.
            if (l.eq.0 .and. s.eq.0.) then
                nrmx=nr
                nrmx=nrmx+mod(nrmx,2)
                goto 77
            endif
        enddo
77    continue
      enddo

      ir=1
c      print*, "Dens Check", ir,lmax,P%dens(0:lmax,ir)
c      do ir=1,10
c        l=0
cc      do ir=1,nr
cc        do l=0,lmax,2
c            print*, "Dens Check", ir, l,l/2+1,P%dens(l/2+1,ir)
cc        enddo
c      enddo


      return
      end subroutine
cccccc



      end module
