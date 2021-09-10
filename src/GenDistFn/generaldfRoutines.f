ccccccccccc
c
c     General Distribute Function routines
c
c       This module contains general routines that are used for both the bulge
c       and halo DF routines
c
cccccccccc

      module GeneralDistDFMod
      use Globs
      use GetPsiMod
      use RootfinderMod
      use GetForceMod
      use TotDensMod

      implicit none


      contains
cccc

      real function GeneralGetDF(energy,ObjType)
      implicit none
      real energy
      real tmax
      real racc,dt,t
      real psi,rmin,rmax,rpsi
      real sum, d2rhodpsi2
      integer i
      integer ObjType

c      print*, "GeneralGetDF Start", energy,ObjType

      tmax = sqrt(energy-DF%psic)
      if(energy .lt. DF%psic) tmax=0.
      dt = tmax/float(DF%nint-1)
      psi = energy

      racc = 1e-8
      call findbrackets(gettotalpsi,psi,rmin,rmax)
c      print*, "Brackets found",rmin,rmax
      call rtbis(gettotalpsi,psi,rmin,rmax,racc,rpsi)
c      print*, "Root found", rpsi
      d2rhodpsi2 =  getd2rhodpsi2(rpsi,ObjType)
      sum = dt*d2rhodpsi2
c      print*, "Initial Test", energy, DF%psic,dt,tmax,sum
c      i=1
      do i=1,nint-2
        t = dt*float(i)
        psi = energy - t*t
c        print*, "testing", i, psi,t
        call findbrackets(gettotalpsi,psi,rmin,rmax)
        call rtbis(gettotalpsi,psi,rmin,rmax,racc,rpsi)
        d2rhodpsi2 =  getd2rhodpsi2(rpsi,ObjType)
        sum = sum + 2.*dt*d2rhodpsi2
      enddo
c      print*, "Gen Get DF", energy, ObjType, sum
      GeneralGetDF = sum/sqrt(8.)/pi/pi
c      print*, "GeneralGetDF Fin", GeneralGetDF

      return
      end function
ccccccccc

ccccccc
      real function getd2rhodpsi2(rad,ObjType)
      implicit none
      integer ObjType
      real rad, Den,DenP,DenPP,Force
      real bbb,ccc,ddd

      real totalden

c      print*, "Getd2rhodpsi2", rad, ObjType
      call GetDensTerms(rad,ObjType,Den,DenP,DenPP)
      Force = gettotalforce(rad)
c      print*, "Force", Force
      totalden = TotDens_Grid(rad,0.)
c      print*, "Total Dens", totalden

      bbb = 4.*pi*totalden*Denp/Force
      ccc = 2.*Denp/rad
      ddd = Denpp
      getd2rhodpsi2 = (bbb + ccc + ddd)/(Force**2.)
c      print*, "Hmmm", rad,ObjType,bbb,ccc,ddd,Force

      return
      END function
cccccccc

cccccc
      subroutine GetDensTerms(rad,ObjType,Den,DenP,DenPP)
      implicit none
      real rad,Den,DenP,DenPP
      integer ObjType
c
      if(ObjType .eq. 1) then
        Den = sersicdens(rad,Bulge)
        Denp = sersicdensprime(rad,Bulge)
        Denpp = sersicdens2prime(rad,Bulge)
      elseif(ObjType .eq. 2) then
        Den = halodensity(rad)
        Denp = halodensprime(rad)
        Denpp = halodens2prime(rad)
      endif
c      print*, "General Dens terms", rad, ObjType,Den,DenP,DenPP
      return
      end subroutine
ccccccccc




      end module
