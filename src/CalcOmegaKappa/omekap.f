ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module OmekapMod
      use Globs
      use GenCompGlobs
      use SplineMod

      implicit none

      Type OmeKapObj
        integer nReq
        real,ALLOCATABLE :: rr(:), psi(:), psirr(:),om(:),om2(:)
        real,ALLOCATABLE :: ak(:),ak2(:)
      end Type

      real a,b,c
      Type(OmeKapObj) OKOb

      contains

ccccccc
      subroutine omekap(r,fom,fka)
      implicit none
      real r,fom,fka
      real r3,r5
      integer kk

c      print*, "calculate omekap",OmekapCallCount,OKOb%nreq
      if(OmekapCallCount.eq. 0) call omekapFirst(fom,fka)
      OmekapCallCount=OmekapCallCount+1

      if(r .gt. OKOb%rr(OKOb%nreq)) then
        r3=a/r**3
        r5=3*b/r**5
        fom=sqrt(r3+r5)
        fka=sqrt(r3-r5)
      else
        call splintd(OKOb%rr,OKOb%om,OKOb%om2,OKOb%nreq,r,fom)
        call splintd(OKOb%rr,OKOb%ak,OKOb%ak2,OKOb%nreq,r,fka)
        if(isnan(fom)) then
            call splintd(OKOb%rr,OKOb%om,OKOb%om2,OKOb%nreq,r,fom)
            write(0,*) fom
            call splintdlong(OKOb%rr,OKOb%om,OKOb%om2,OKOb%nreq,r,fom)
            write(0,*) fom
            open(file='check',unit=30,status='replace')
            do kk=1,OKOb%nreq
                write(30,*) OKOb%rr(kk),OKOb%om(kk),OKOb%om2(kk)
            enddo
            stop
        endif
      endif



      return
      end subroutine
ccccccccc

ccccccc
      subroutine omekapFirst(fom,fka)
      implicit none
      integer ir
      real fom,fka
      real rx,r3,r5


c      print*, "First omekap first", r
c     read omega, potential and potential'' from the frequencies file,
      call ReadFreqs()
c     extrapolate omega (linearly) and potential (quadratically) to zero
      OKOb%om(1)=2*OKOb%om(2)-OKOb%om(3)
      OKOb%psi(1)=(4*OKOb%psi(2)-OKOb%psi(3))/3.
      OKOb%psirr(1)=(4*OKOb%psirr(2)-OKOb%psirr(3))/3.
      OKOb%rr(1)=0.

c     spline-fit pot(r), and then use the 2nd derivatives of the
c     potential in the table of kappa
      call splined(OKOb%rr,OKOb%om,OKOb%nreq,1.e32,1.e32,OKOb%om2)


c     calculate epicycle frequencies
      OKOb%ak(1)=2*OKOb%om(1)
      do ir=2,OKOb%nreq
        OKOb%ak(ir)=sqrt(max(0.,OKOb%psirr(ir)+3*OKOb%om(ir)**2))
      enddo
      call splined(OKOb%rr,OKOb%ak,OKOb%nreq,0.,1.e32,OKOb%ak2)
c      call splined(OKOb%rr,OKOb%ak,OKOb%nreq,1.e32,1.e32,OKOb%ak2)
c     a and b are coefficients of potential -a/r -b/r^3 +c
c     which give omeka and kappa as observed
c     at the outer radius of the model---
c     these are used to extrapolate if needed.
c     in this potential om^2=a/r^3+3b/r^5, kap^2=a/r^3-3b/r^5
      a=OKOb%rr(OKOb%nreq)**3/2.*(OKOb%om(OKOb%nreq)**2+OKOb%ak(OKOb%nreq)**2)
      b=OKOb%rr(OKOb%nreq)**5/6.*(OKOb%om(OKOb%nreq)**2-OKOb%ak(OKOb%nreq)**2)
      c=a/OKOb%rr(OKOb%nreq)**3+b/OKOb%rr(OKOb%nreq)**5+OKOb%psi(OKOb%nreq)


      open(11,file='omekap2.dat',status='replace')
      do ir=1,OKOb%nreq
        write(11,'(7g16.8)') OKOb%rr(ir),OKOb%om(ir),OKOb%om2(ir)
     &              ,OKOb%ak(ir),OKOb%ak2(ir),
     &              OKOb%psi(ir),OKOb%psirr(ir)
      enddo
      do ir=1,10
        rx=OKOb%rr(OKOb%nreq)*(1+0.1*ir)
        r3=a/rx**3
        r5=3*b/rx**5
        fom=sqrt(r3+r5)
        fka=sqrt(r3-r5)
        write(11,'(7g16.8)') rx,fom,0.,fka,0.,
     &              c-(r3+r5/3.)*rx*rx,-2*r3-4*r5
      enddo
      close(11)
      write(0,*) 'wrote a table of frequencies in file omekap.dat.'
      return
      end subroutine
cccccccc

ccccccc
      subroutine ReadFreqs()
      implicit none

      integer i
      integer imax, nReqs
      parameter(imax=int(1e6))
      real t,vc
      real t1,t2,t3


      open(10,file='freqdbh.dat',status='old')
      read(10,*)
      read(10,*)
      read(10,*)
c           Figure out the number of records in freqdbh.dat
      do i=1,imax
        read(10,*,end=99)
      enddo
99    nReqs=int((i-1)/2)+1
      print*, "number of records in freqdbh.dat",nReqs
      rewind(10)

c           ALLOCATE ALL THE DiskDF ARRAYS
      ALLOCATE(OKOb%rr(nReqs))
      ALLOCATE(OKOb%psi(nReqs))
      ALLOCATE(OKOb%psirr(nReqs))
      ALLOCATE(OKOb%om(nReqs))
      ALLOCATE(OKOb%om2(nReqs))
      ALLOCATE(OKOb%ak(nReqs))
      ALLOCATE(OKOb%ak2(nReqs))
      OKOb%nReq=nReqs

      read(10,*)
      read(10,*)
      read(10,*)
      nReqs=1
c     read only every other step, since remainder was linearly
c     interpolated anyway
      do i=1,imax
c        print*, "omekap read in", i
c        read(10,*,end=98) OKOb%rr(nreqs+1),t,t,t,vc,t,t,OKOb%psi(nreqs+1)
c     &          ,OKOb%psirr(nreqs+1)
        read(10,*,end=98) t1,t,t,t,vc,t,t,t2
     &          ,t3
c        print*, "omekap read in", i,t1,t2,t3,nReqs
        OKOb%rr(nreqs+1)=t1
        OKOb%psi(nreqs+1)=t2
        OKOb%psirr(nreqs+1)=t3
        OKOb%om(nreqs+1)=vc/OKOb%rr(nreqs+1)
        read(10,*,end=98)
        nReqs=nReqs+1
      enddo

98    close(10)

      return
      end subroutine
cccccc
      end module
