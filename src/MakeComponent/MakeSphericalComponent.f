ccccccccccccc
c
c     Make component
c
c       This module contains routines that are used for calculating position
c       and velocities drawn from random distributions
cccccccccccc

      module MakeSphComponentMod
      use Globs
      use GenCompGlobs
      use RandomMod
      use CalcPotGridMod
      use ForceFromGridMod
      implicit none

      contains


cccccccccc
      subroutine GetTestPos(P,pCheck,DensFunc)
      implicit none
      Type(Particle) P
      real u1,v1,R,Z,ZAbs
      logical pCheck
      real rhotest, rhoran,Phi
      real DensFunc
      external DensFunc

c     Get test R and Z
      u1=u1max*ran3(idum)
      v1=2.*v1max*(ran3(idum)-0.5)
c      print*, "Test Pos Limits", u1,v1,u1max,v1max
      R=u1
      Z=R*tan(v1)
      ZAbs=abs(Z)
c       Check that Z is not out of bounds
      if(ZAbs .gt. 2.*rtrunc) then
        pCheck=.True.
        return
      endif
c       Check that the density is large enough
      rhotest=DensFunc(R,Z)*(R*R+Z*Z)
      if(rhotest .lt. rhomin) then
        pCheck=.True.
        return
      endif
c       Make sure we are drawing from the density distribution
      rhoran=(rhomax-rhomin)*ran3(idum)
      if(rhoran .gt. rhotest) then
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

ccccccc
      subroutine GetTestVel(P,pCheck,fcut,dffunc)
      implicit none
      Type(Particle) P
      logical pCheck
      real psi,vmax,vmax2,E
      real f0,frand,fmax
      real v2,vR,vp,vz
      integer j
      real cph,sph
      real fcut
      real dffunc
      external dffunc

c       Check the potential
      psi=pot(P%R,P%Pos(3))
      if(psi .lt. DF%psic) then
        pCheck=.True.
        return
      endif
c       Get the maximum velocity
      vmax2=2.*(psi-DF%psic)
      vmax=sqrt(vmax2)
      fmax=dffunc(psi)-fcut

      do j=1, nPart
99      continue
        vR=2.*vmax*(ran3(idum)-0.5)     !Select random vector
        vp=2.*vmax*(ran3(idum)-0.5)
        vz=2.*vmax*(ran3(idum)-0.5)
        v2=vR*vR+vp*vp+vz*vz
        if(v2 .gt. vmax2) goto 99       !make sure that v2 is lower than vmax
        E=psi-0.5*v2                    !Get the energy
        f0=dffunc(E)-fcut          !Find f0
        frand=fmax*ran3(idum)           !Make sure f0 is drawn from f
        if(frand .le. f0) goto 80
      enddo
      pCheck=.True.                     !If it takes over nPart tries, use a new position
      return
80    continue
c       Get the direction of tangential velocity
      if(ran3(idum) .lt. StreamingFrac) then
        vp=abs(vp)
      else
        vp=-abs(vp)
      endif

c       Assign the velocities
      cph=P%Pos(1)/P%R
      sph=P%Pos(2)/P%R
      P%Vel(1)=vR*cph-vp*sph
      P%Vel(2)=vR*sph+vp*cph
      P%Vel(3)=vz

      return
      end subroutine
cccccccc

cccccccc
      subroutine EnergyCalc(P)
      implicit none
      Type(Particle) P
      real fr,fz
c
      Kinetic=Kinetic+0.5*pMass*(P%Vel(1)**2.+P%Vel(2)**2.+P%Vel(3)**2.)
      call force(P%R,P%Pos(3),fr,fz,TotPot)
      potential=potential+pMass*(fr*P%R+fz*P%Pos(3))

      return
      end subroutine
cccccccc

      end module
