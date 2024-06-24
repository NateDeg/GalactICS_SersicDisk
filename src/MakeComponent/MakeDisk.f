ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module MakeDiskMod
      use Globs
      use GenCompGlobs
      use RandomMod
      use DiskDensFMod
      use DiskSigmaMod
      use OmekapMod
      use diskdfezMod
      use ForceFromGridMod


      implicit none

      contains

ccccccc
      subroutine MakeDisk()
      implicit none
      integer i,count
      Type(Particle) PTest
      logical pCheck
      integer DensTrials,VelTrials,funit
      character(30) fname

      real RTest,fr,fz,fr2,fz2

      funit=10
      if(DiskSwitch .eq. 1) then
        fname='rcirc.disk1'
      elseif(DiskSwitch .eq. 2) then
        fname='rcirc.disk2'
      endif
      open(funit,file=trim(fname),status='replace')

      count=0
      print*, "Calculating disk positions and velocities"
      print*, "Sigma", DUse%sigr01,DUse%disksr1
     &              ,DUse%sigr02,DUse%disksr2


c      do i=1,40
c        RTest=real(i)-0.5
c        call force(RTest,0.,fr,fz,TotPot)
c        call force(RTest,0.,fr2,fz2,HaloPot)
c        print*, i, RTest,fr,-fr*RTest,sqrt(-fr*Rtest)
c     &              ,fr2,sqrt(-fr2*Rtest)
c      enddo


      DensTrials=0
      VelTrials=0
      do i=1, nPart
cc      do i=1, 2
100     pCheck=.False.

        count=count+1
ccc        print*, i,count
        call GetDiskTestPos(PTest,pCheck,DensTrials)
        if(pCheck) goto 100
        call GetDiskTestVel(PTest,pCheck,VelTrials,funit)
        if(pCheck) goto 100
        call EnergyCalc(PTest)
        P(i)=PTest
        P(i)%Mass=pMass
        if( mod(i,1000) .eq. 0) write(*,101,advance='no')"."
      enddo
      print*, " "
      print*, "2*kinetic/potential",2.*kinetic/potential
      print*, "kineticz,potentialz",kineticz,potentialz
      print*, "2*kinetizc/potentialz",2.*kineticz/potentialz
      print*, " "
      print*, "number of density trials ",DensTrials
      print*, "number of velocity trials ",VelTrials
      close(funit)

101   format(A)

      return
      end subroutine
ccccccccc


cccccccccc
      subroutine GetDiskTestPos(P,pCheck,DenTrials)
      implicit none
      Type(Particle) P
      logical pCheck
      real u1,v1,R,Z
      real zcon
      real rhoguess,rhotst,rhoran,Phi
      integer DenTrials

      real R1, R2, R3

c     Get test R and Z
100   R=2.*DUse%rtrunc

      u1=-ran3(idum)
      v1=2.*(ran3(idum)-0.5)


c      u1=-0.999
      R1=invu_New2(u1)
c      R1=invu(u1)
c      R2=invu_New(u1)
c      R3=R
      R=R1
c      print*, "Invu Comparison", R1
c      print*, "After calling invu", u1,R
      R=DUse%Rd*R


c      print*, "Normalized R", u1,R
      Z=DUse%Zd*atanh(v1)


      if(R .gt. DUse%rtrunc .or. R .le. 0.) goto 100
      if(abs(Z) .gt. 10.*DUse%Zd) goto 100

      zcon=cosh(z/DUse%Zd)
c      rhoguess=exp(-R/DUse%Rd)/(zcon*zcon)
      rhoguess=exp(-(R/DUse%Rd)**(1./DUse%ndisk))/(zcon*zcon)
      rhotst=diskdensf(R,Z,DUse)
      rhotst=rhotst/rhoguess
      DenTrials=DenTrials+1

c      if (R .lt. DUse%Rd) then
c        print*, "low R Check",R, rhoguess,diskdensf(R,Z,DUse),rhotst
c     &          ,rhomax,rhomin
c      endif

c      print*, "Dens Tests", u1, R, rhoguess
c     &          , rhotst, rhotst*rhoguess, DenTrials
c     &          , rhomin, rhomax

      if(rhotst .lt. rhomin) then
        pCheck=.True.
        return
      endif
c       Make sure we are drawing from the density distribution
      rhoran=(rhomax-rhomin)*ran3(idum)
c      print*, "Accept/reject", rhoran, rhotst,rhomax,rhomin
      if(rhoran .gt. rhotst) then
        pCheck=.True.
        return
      endif

      Phi=2.*Pi*ran3(idum)
      P%R=R
      P%Pos(1)=R*cos(Phi)
      P%Pos(2)=R*sin(Phi)
      P%Pos(3)=Z


      return
      end subroutine
cccccc

cccccc
      subroutine GetDiskTestVel(P,pCheck,VelTrials,funit)
      implicit none
      Type(Particle) P
      logical pCheck
      real omega,kappa
      real vphimax,vsigR,vsigp,vsigz,vphimaxold
      real fmax,f0,frand
      real gr,gp,gz,g2
      real vR,vp,vz
      real cph,sph,angmomz,rcyl,rcir
      integer VelTrials,funit



      call omekap(P%R,omega,kappa)
      vphimax = omega*P%R
      vsigR = sqrt(sigr2(P%R,DUse))
      vsigp = kappa/(2.0*omega)*vsigR
      vsigz = sqrt(sigz2(P%R,DUse))
      vphimaxold = vphimax
      fmax=1.1*FindMax1(P%R,P%Pos(3),vphimax,vsigp)

c      print*, "new fmax",fmax
      if(fmax .le. 0.) then
c        print*, "bad fmax", P%R,P%Pos(3),vphimax,vsigp
        pCheck=.True.
        return
      endif
      vphimax=vphimaxold


 100  g2=999.
      gr= 8.*(ran3(idum)-0.5)
      gp=16.*(ran3(idum)-0.5)
      gz=8.*(ran3(idum)-0.5)
      g2= gr*gr/16. + gp*gp/64. + gz*gz/16.
c      print*, "Gtest", gr,gp,gz,g2
      if(g2 .ge. 1.) goto 100

      vR=broadcon*vsigR*gr
      vP=vphimax+broadcon*vsigp*gp
      vz=broadcon*vsigz*gz
      f0=Fdisk(vR,vp,vz,P%R,P%Pos(3))
      frand=fmax*ran3(idum)
c      if(f0 .gt. fmax) then
c        print*, "f0>fmax at R,z=", P%R,P%Pos(3)
c        print*, "vr,vp,vz=", vr,vp,vz
c        print*, "vphimax,f0,fmax",vphimax,f0,fmax
c      endif
      VelTrials=VelTrials+1
c      if(P%R .ge. 4.2 .and. P%R .le. 4.3) then
c        print*, "R trial", VelTrials, P%R, P%Pos(3)
c     &          ,vphimax,vsigp
c     &          ,vR,vP,vz,f0,frand,fmax
c      endif


      if(frand .gt. f0) goto 100

      vphimax=vp-broadcon*vsigp*gp
      vP=vphimax+broadcon*vsigp*gp
      cph=P%Pos(1)/P%R
      sph=P%Pos(2)/P%R
      P%Vel(1)=vR*cph - vp*sph
      P%Vel(2)=vR*sph + vp*cph
      P%Vel(3)=vz

      angmomz = P%Vel(2)*P%Pos(1)
     &              - P%Vel(1)*P%Pos(2)
      rcyl = sqrt(P%Pos(1)*P%Pos(1)
     &              +P%Pos(2)*P%Pos(2))
      rcir = rcirc(angmomz,DUse)

      write(funit,*) rcyl,rcir,angmomz,vp,vsigp,vphimax,f0,vsigz


      return
      end subroutine
ccccccc


cccccccc
      subroutine EnergyCalc(P)
      implicit none
      Type(Particle) P
      real fr,fz
      real rcyl
c
c      print*, "Energy Calc"
      rcyl=sqrt(P%Pos(1)**2.+P%Pos(2)**2.)
      Kinetic=Kinetic+0.5*pMass*(P%Vel(1)**2.+P%Vel(2)**2.+P%Vel(3)**2.)
      call force(rcyl,P%Pos(3),fr,fz,TotPot)
      potential=potential+pMass*(fr*rcyl+fz*P%Pos(3))
c      print*, "Quick Test", rcyl,P%Pos,Kinetic,potential,fr,fz
      if(P%R .gt. DUse%rdisk) then
        potentialz=potentialz+pMass*(P%Pos(3)*fz)
        kineticz=kineticz+0.5*pMass*(P%Vel(3)*P%Vel(3))
      endif

      return
      end subroutine
cccccccc



ccccccccc
      real function invu(u)
      implicit none
      real u
      real rg,dr,eps,rnew
      integer i

      rg=1.
      dr=1.
      eps=1.e-6
      do i=1,20
        rnew=rg-(-(1.+rg)*exp(-rg)-u)
     &          /(rg*exp(-rg))
        dr=abs(rnew-rg)
        rg=rnew

c        print*, "invu test", i, rg,dr
        if(dr .lt. eps) goto 200
      enddo
      print*, "Warning R did not converge"
200   continue
      invu=rg
      return
      end function
ccccccccc




ccccccccc
      real function invu_New(u)
      use GammaFnRoutines
      implicit none
      real u
      real rg,dr,eps,rnew
      integer i

      real n, rP
      real F,G

      n=DUse%ndisk

      rg=1.
      dr=1.
      eps=1.e-6
      rnew=10.
      do i=1,20
        rP=rg
        F=rg*exp(-rP**(1./n))
        G=-gammq(2.*n, rp**(1./n))

        rnew=rg-((G-u)/F)
        dr=abs(rnew-rg)
c        print*, "invu_New loop", i,rg,rP,rnew,dr,F,G

        rg=rnew
c        print*, "invu test", i, rg,dr
        if(dr .lt. eps) goto 200
      enddo
      print*, "Warning R did not converge"
200   continue
      invu_New=rg
      return
      end function
ccccccccc


ccccccccc
      real function invu_New2(u)
      use GammaFnRoutines
      implicit none
      real u
      real rg,dr,eps,rnew
      integer i

      real n
      real G
      real Rmin, Rmax
      real Rlow,RHigh,RMid, FLow,FHigh,FMid

      eps=1.e-5
      RMin=0.
      RMax=DUse%rtrunc
      n=DUse%ndisk
      if(n .gt. 1) RMax=15.**n

      RLow=RMin
      RHigh=RMax


      FLow=RadialSersicFn(RLow,n,DUse%Rd)-u

      FHigh=RadialSersicFn(RHigh,n,DUse%Rd)-u

      if(FHigh*FLow .gt. 0.) then
c        print*, "Root is not bracketed", u, Rlow,
c     &          FLow, RHigh,FHigh, n
c     &          ,RadialSersicFn(RHigh,n,DUse%Rd)
c     &          ,RadialSersicFn(RLow,n,DUse%Rd)
        invu_New2=5.*RMax
        return
      endif

      do i=1, 100
        RMid=(Rhigh+RLow)/2.

        FMid=RadialSersicFn(RMid,n,DUse%Rd)-u
c        print*, i, RLow,FLow, RMid,FMid, RHigh,FHigh

        if(abs(FMid) .lt. eps) goto 200

        if(FLow*FMid .gt. 0.) then
            FLow=FMid
            RLow=RMid
        else
            FHigh=FMid
            RHigh=RMid
        endif
      enddo
      print*, "Warning R did not converge"
200   continue
      invu_New2=RMid
c      print*, "invu_new2", u, RMid
      return
      end function
ccccccccc


ccccc
c
      real function RadialSersicFn(RS,n,Rd)
      use GammaFnRoutines
      implicit none
      real RS, n, Rd

c      print*, "RadialSersicFn", RS,n,Rd
      RadialSersicFn=-gammq(2.*n,(RS)**(1./n))
c      print*, "RadialSersicFn", RS,n,(RS)**(1./n),RadialSersicFn
      return
      end function
cccccccc

cccc
      real function FindMax1(R,z,vpmax,vsigp)
      implicit none
      real R,z,vpmax,vsigp
      integer flag
      real dv,vpm,vpmold,v0,v1
      real f0,f1,ftmp,fmid
      real zero

c      print*, "Find Max1 Test", vpm,vpmax
      zero=0.
      dv=0.1*vsigp
      vpm=vpmax
      v0=vpm-dv
      v1=vpm+dv
c      print*, "Find Max1 Test", vpm,vpmax

      f0=FDisk(0.0,v0,0.0,R,z)
      fmid=FDisk(0.0,vpm,0.0,R,z)
      f1=FDisk(0.0,v1,0.0,R,z)
c      print*, "First Check", R,z,f0,fmid,f1
      if(fmid. ge. f0 .and. fmid .ge. f1) then
        FindMax1=fmid
c        print*,"fmid is best", f0,f1,fmid
        return
      endif

      if(f0 .gt. f1) then
        ftmp=f0
        f0=f1
        f1=ftmp
        v1=v0
        dv=-dv
      endif
      vpm=v1

      flag=1
 101  continue
      dv=dv*2.
      vpmold=vpm
      vpm=vpm+dv
      f0=f1
      f1=FDisk(0.0,vpm,0.0,R,z)
c      print*, "Testing", f0,f1,dv
      if(f1 .gt. f0) goto 101

c      vpmax=vpmold
      FindMax1=f0
c      print*, "hmmm", f0,1.1*f0
      return
      end function
cccccccc


cccccccc
      real function FDisk(vr,vp,vz,R,z)
      implicit none
      real vr,vp,vz,R,z
      real vr0,vp0,vz0,R0,z0
      real T 

      vr0=vr
      vp0=vp
      vz0=vz
      R0=R
      z0=z
      T=diskdf5ez(vr0,vp0,vz0,R,z,DUse)
      FDisk=T

      return
      end function
ccccccc



      end module
