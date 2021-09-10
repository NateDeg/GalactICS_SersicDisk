ccccccccc
c
c     Calc Potential
c
c       This module uses the potential arrays to calculate
c       the total potential at a particular point in s,z
ccccccccc
c
      module CalcPotGridMod
      use Globs
      use LegendreRoutines
      use AppDiskPotMod
      use AppGasPotMod
      use BHGeneralMod

      implicit none

      contains
c
ccccccccccc
      real function pot(s,z)
      implicit none
      real s,z
      real p
      real apd,apd2,agp
      real rad,bhp
c
      p=potFromGrid(s,z,TotPot)
      rad=sqrt(s*s+z*z)
c      print*, "Pot from grid", p

      if( DiskFlag1) then
         apd = appdiskpot(s,z,D1)
         p = p + apd
      endif
      if( Diskflag2) then
         apd2 = appdiskpot(s,z,D2)
         p = p + apd2
      endif
      if( GasFlag ) then
         agp = appgaspot(s,z,Gas)
         p = p + agp
      endif
      if( BHFlag) then
        rad = sqrt(s*s + z*z)
        bhp = getbhpsi(rad)
        pot = pot + bhp
      endif

      pot=p
c      print*, "Pot Test", s,z,p, apd,apd2,agp


      if(isnan(pot).or.pot+1.eq.pot) then
        write(*,*)
        write(*,*) 'nan for pot'
        write(*,*) s,z,agp,apd2,apd
        stop
      endif

      return
      end function
ccccccccc


ccccccccc
      real function potFromGrid(s,z,PObj)
      real r,s,z
      real p
      integer ihi,lmax,l
      real r1,r2,t,tm1,costheta
      Type(PotObj) PObj
c
c      print*, "Pot From Grid", s,z
c
      r=sqrt(s*s+z*z)

      if(r.eq.0.) then
         potFromGrid = PObj%pot(1,0)/sqrt(4.*pi)
         return
      endif
      ihi=int(r/dr)+1
      if(ihi.lt.1) ihi=1
      if (ihi.gt.nr) ihi=nr
      r1=dr*(real(ihi)-1.)
      r2=dr*real(ihi)
      t=(r-r1)/(r2-r1)
      tm1 = 1.0 - t
      if (r.eq.0.) then
         lmax=0
         costheta=0
      else
         costheta=z/r
         lmax=lmaxx
      endif
      p=0
      do l=lmax,0,-2
         p=p+plgndr1(l,costheta)*plcon(l)*
     +        (t*PObj%pot(l/2+1,ihi)+ tm1*PObj%pot(l/2+1,ihi-1))
c        print*, "Legendre", l, p,plgndr1(l,costheta),plcon(l)
c     &          ,t,PObj%pot(l/2+1,ihi),tm1,PObj%pot(l/2+1,ihi-1)
      enddo

c      print*, "Pot from grid", s,z, r,r1,r2,ihi,p

      if(isnan(p).or.p+1.eq.p) then
         write(*,*)
         write(*,*) 'nan for pot grid'
         write(*,*) 's,z,pot',s,z,p
         stop
      endif
      potFromGrid=p



      return
      end function

ccccccccc



ccccccccc
      real function FR2FromGrid(s,z,PObj)
      real r,s,z
      real p
      integer ihi,lmax,l
      real r1,r2,t,tm1,costheta
      Type(PotObj) PObj

      r=sqrt(s*s+z*z)

      if(r.eq.0.) then
        FR2FromGrid = PObj%Fr2(1,0)/sqrt(4.*pi)
        return
      endif
      ihi=int(r/dr)+1
      if(ihi.lt.1) ihi=1
      if (ihi.gt.nr) ihi=nr
      r1=dr*(real(ihi)-1.)
      r2=dr*real(ihi)
      t=(r-r1)/(r2-r1)
      tm1 = 1.0 - t
      if (r.eq.0.) then
        lmax=0
        costheta=0
      else
        costheta=z/r
        lmax=lmaxx
      endif
      p=0
      do l=lmax,0,-2
        p=p+plgndr1(l,costheta)*plcon(l)*
     +        (t*PObj%Fr2(l/2+1,ihi)+ tm1*PObj%Fr2(l/2+1,ihi-1))
      enddo

      if(isnan(p).or.p+1.eq.p) then
        write(*,*)
        write(*,*) 'nan for pot'
        write(*,*) 's,z,pot',s,z,p
        stop
      endif
      FR2FromGrid=p

      return
      end function

ccccccccc


      end module

