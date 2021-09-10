ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines for calculating quantities for
c       NFW (double-power law actually) halo in the GalactICS code
cccccccccccc

      module MakeGasMod
      use Globs
      use GenCompGlobs
      use RandomMod
      use GasZMod
      use GasSurfDensMod
      use CalcPotGridMod
      use ForceFromGridMod
      use SplineMod

      implicit none

      contains

ccccccc
      subroutine MakeGas()
      implicit none
      integer i,j,count
      Type(Particle) PTest
      logical pCheck

      count=0
      print*, "Calculating gas positions and velocities",Adio_IsoTempSwitch
      j=0
      do i=1, nPart
c      do i=1, 1
100     pCheck=.False.
        count=count+1
c        print*, i,count
        call GetGasPos(PTest,pCheck)
        if(pCheck) goto 100
        j=j+1
        call GetGasVel(PTest)
        P(j)=PTest
        P(j)%Mass=gasdiskparticlemass
        if( mod(j,1000) .eq. 0) write(*,101,advance='no')"."
        if(j .eq. nPart) goto 200
c           Section to force vertical symmetry
c        j=j+1
c        PTest%Pos(3)=-PTest%Pos(3)
c        P(j)=PTest
c        P(j)%Mass=gasdiskparticlemass
c        if( mod(j,1000) .eq. 0) write(*,101,advance='no')"."
c        if(j .eq. nPart) goto 200
      enddo
101   format(A)
200   continue

      return
      end subroutine
ccccccccc

ccccccc
      subroutine GetGasPos(P,pCheck)
      implicit none
      Type(Particle) P
      logical pCheck
      real uu,zz
      real rcyl,zgas,rphi
      real f,f1r,f2,ratio,rnd
      real psi,psi0,zero

      zero=0.
      uu=-(umin + (1.-umin)*ran3(idum))
      rcyl=Gas%rgas*getrcyl(uu)
      zgas=getzgas(rcyl,Gas)
      rphi=2.*pi*ran3(idum)
      call gassurfdens(rcyl,f,f1r,f2,Gas)
      ratio = f/exp(-rcyl/Gas%rgas)/Gas%gasconst
      rnd = ran3(idum)
c      print*, "First Check", rcyl,zgas,ratio,rnd
      if(rnd .gt. ratio) then       !Check Rcyl
        pCheck=.True.
        return
      endif

      zz=5.*zgas*(-1. + 2.*ran3(idum))
      psi = pot(rcyl,zz)
      psi0 = pot(rcyl,zero)
      ratio = exp((psi-psi0)/Gas%zgas0)
      rnd = ran3(idum)
c      print*, "Vert Check", zz,psi,psi0,zgas,ratio,rnd
      if(rnd .gt. ratio) then           !Check z
        pCheck=.True.
        return
      endif
      P%Pos(1)= rcyl*cos(rphi)
      P%Pos(2)= rcyl*sin(rphi)
      P%Pos(3)= zz

c      print*, "Final Select Pos", P%Pos
      return
      end subroutine
ccccccccc


cccccccc
      subroutine GetGasVel(P)
      implicit none
      Type(Particle) P
      real rcyl,vc2

      rcyl=sqrt(P%Pos(1)**2.+P%Pos(2)**2.)
      call getvcirc2(rcyl,P%Pos(3),vc2)
      if(vc2 .gt. 0.) then
        P%Vel(1)=(-P%Pos(2)/rcyl)*sqrt(vc2)
        P%Vel(2)=(P%Pos(1)/rcyl)*sqrt(vc2)
      else
        P%Vel(1)=0.
        P%Vel(2)=0.
      endif

      if(Adio_IsoTempSwitch .eq. 1) then
        P%Vel(3)=sqrt(Gas%zgas0/(abs(Gas%Gamma)-1.))
c        print*, "hmmm",P%Vel(3),Gas%zgas0,abs(Gas%Gamma)-1.
      elseif(Adio_IsoTempSwitch .eq. 2) then
        P%Vel(3)=sqrt(Gas%zgas0)
      else
        P%Vel(3)=0.
      endif


      return
      end subroutine
cccccccc


ccccccc
      real function getrcyl(u)
      implicit none
      real r, eps
      integer k
      real du,dudr,u

      r = 1.
      eps = 0.000001

      do k=1,25
        du = u + (1. + r)*exp(-r)
        if(abs(du).lt.eps) then
            getrcyl = r
            return
        endif
        dudr = r*exp(-r)
        r = r + du/dudr
      enddo
      print*, "No Conversion in getrycl() in MakeGas",u
      stop

      return
      end function
ccccccc

cccccccc
      subroutine getvcirc2(r,z,vcirc2)
      implicit none
      real r,z,vcirc2
      real fs,fz,f1r,f2,gn,dgn,gsd

      call force(r,z,fs,fz,TotPot)
c      call force(r,0.,fs,fz,TotPot)
      call splintd(Gas%gasnormrad,Gas%gasnorm
     &              ,Gas%gasnorm2,Gas%nrsplg,r,gn)
      call splintd(Gas%gasnormrad,Gas%dgasnorm
     &              ,Gas%dgasnorm2,Gas%nrsplg,r,dgn)
      call gassurfdens(r,gsd,f1r,f2,Gas)
      vcirc2 = -r*fs + r*Gas%zgas0*(r*f1r/gsd - dgn/gn)
c      if(r .le. 0.2) then
c        print*, "hmmm", r,z,vcirc2
c      endif

      return
      end subroutine
ccccccccccc


      end module
