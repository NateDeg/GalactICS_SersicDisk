cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module initializes the values for the gendisk
c       program
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module iniGenDiskMod
      use Globs
      use SplineMod
      use OmekapMod
      use DiskSigmaMod
      use DiskDensFMod
      use MassRingMod
      use SimpsonIntMod



      implicit none

      contains

cccccccc
      subroutine IniGenDisk()
      implicit none
      real fom,fka
      real sigr,sigden,sigrcrit,qtoomre,rtoomre
      integer ntoomre
      real xlindbladmax,xlindblad,rxlindblad,x50,rcrise,xparam
      integer i,nn
      real Rini

      print*, "Initialize Gen Disk"
      ALLOCATE(P(nPart))

c       Run the spline fit on the disk being used
      OmekapCallCount=0
      broadcon=1.0

      print*, "Disk Ini, 1st splined call",DUse%nrspl+1
      call splined(DUse%rr(0:DUse%nrspl),DUse%fdrat(0:DUse%nrspl)
     &              ,DUse%nrspl+1,1.e32,1.e32,DUse%drat2(0:DUse%nrspl))
      print*, "Disk Ini, 2nd splined call",DUse%nrspl+1
      call splined(DUse%rr(0:DUse%nrspl),DUse%fszrat(0:DUse%nrspl)
     &          ,DUse%nrspl+1,1.e32,1.e32,DUse%szrat2(0:DUse%nrspl))

      DUse%rfid = 2.5*DUse%rdisk

      call omekap(DUse%rfid,fom,fka)
      sigr = sqrt(sigr2(DUse%rfid,DUse))
      sigden = diskdensf(DUse%rfid,0.0,DUse)*2.0*DUse%zdisk

      open(20,file="SigCheck.txt", status='replace')
      write(20,*) "R    fszrat      szrat2    sigz2   sigr"
      do i=0, DUse%nrspl
        print*, "disk sigz check", DUse%rr(i), DUse%fszrat(i)
     &          ,DUse%szrat2(i), sqrt(sigz2(DUse%rr(i),DUse))
     &          ,sqrt(sigr2(DUse%rr(i),DUse))
        write(20,*)DUse%rr(i), DUse%fszrat(i)
     &          ,DUse%szrat2(i), sqrt(sigz2(DUse%rr(i),DUse))
     &          ,sqrt(sigr2(DUse%rr(i),DUse))
      enddo
      close(20)

c
c a close estimate of the disk surface density
c
      sigrcrit = 3.36*sigden/fka
      qtoomre = sigr/sigrcrit


      open(12,file='toomre.dat',status='replace')
      ntoomre = int((DUse%outdisk + 2*DUse%drtrunc)/dr)
      xlindbladmax = 0.
      x50 = 0.
      do i=0, ntoomre
        rtoomre = float(i)*dr
        call omekap(rtoomre,fom,fka)
        sigr = sqrt(sigr2(rtoomre,DUse))
        sigden = diskdensf(rtoomre,0.0,DUse)*2.0*DUse%zdisk
        sigrcrit = 3.36*sigden/fka
        qtoomre = sigr/sigrcrit
        xparam = fka*fka*rtoomre/6.283/sigden
        rcrise = fka*fka/2./fom/fom - 1.
        xlindblad = fom - fka/2.
        if(xlindblad.gt.xlindbladmax.and.i.gt.1) then
            xlindbladmax = xlindblad
            rxlindblad = rtoomre
        endif
        write(12,666) rtoomre, qtoomre, xparam, fom,fka,rcrise,sigr
      enddo
      close(12)
 666  format(7f16.3)
      open(file='stability.out',unit=10,status='replace')
      write(10,*) xlindbladmax,rxlindblad,x50
      close(10)

      nn=128
      Rini=0.
      DUse%rd = 1.2*DUse%rdisk
      DUse%zd = 1.2*DUse%zdisk
      DUse%rtrunc = (DUse%outdisk + 2.0*DUse%drtrunc)
c      print*, "Trial 1",MassRing(DUse%rtrunc)
      call BasicSimpsonInt(MassRing,Rini,DUse%rtrunc,nn,DUse%DiskMass)
      DUse%DiskMass=DUse%DiskMass*4.*Pi
c      print*, "Disk Mass Test",DUse%DiskMass
      pMass=DUse%diskmass/real(nPart)

      call FindRhoMaxD()
      kinetic=0.
      potential=0.
      kineticz=0.
      potentialz=0.



      return
      end subroutine
ccccccc



cccccc
      subroutine FindRhoMaxD()
      implicit none
      integer i
      real rcyl,z
      real dr,rhomax1
      integer nSteps
      real rhoguess,rhotst
c
      rhomax1=0.
      nSteps=100
      dr =DUse%rtrunc/real(nSteps)
      do i=1,nSteps
        rcyl=real(i)*dr
        z=0.
        rhoguess=exp(-(rcyl/DUse%rd)**(1./DUse%ndisk))
        rhotst=diskdensf(rcyl,z,DUse)
c        print*, "Rhotest", rcyl,rhotst,rhoguess
        rhotst=rhotst/rhoguess
c        print*, "Rhotest2", rcyl,rhotst,rhoguess
        if(rhotst .gt. rhomax1) rhomax1=rhotst
c        print*, "hmmm ini", rcyl,z,rhocur,rhomax1
      enddo
c      print*, "rhomax1=", rhomax1
      rhomax=1.2*rhomax1
      rhomin=0.
c      print*, "rhomax1=", rhomax
c
      return
      end subroutine
cccccc


      end module
