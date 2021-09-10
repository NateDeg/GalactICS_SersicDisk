cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module calculates the potential for the galaxy
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module TotPotCalcMod
      use Globs
      use GetPsiMod
      use GetForceMod
      use GasZGridMod
      use DistDFMod
      use BulgeDFMod
      use BulgeDensPsiMod
      use HaloDFMod
      use HaloDensPsiMod
      use GasNormMod
      use GridDensMod
      use SimpsonIntMod
      use BHGeneralMod


      implicit none

      contains

ccccccc
      subroutine CalcPot()
      implicit none

      print*, "Calculating potential"
      call PotCalcIni()         !Initialize potential arrays
      call AuxillaryIni()       !Initialize DFs as well as new scale heights and densities
      call PotIterate()         !Run the main potential loop
c      print*, "Quick Dens Test",TotPot%dens(0:lmaxx,1)

      return
      end subroutine
ccccccc

ccccccccc
      subroutine PotCalcIni()
      implicit none

      integer i,l
      real r
      print*, 'Potential Initialize'
      TotPot%pot(1,0)=gettotalpsi(0.0)*sqrt(4.*pi)
      do i=1, nr
        r=real(i)*dr
        TotPot%pot(1,i) = sqrt(4.*pi)*(gettotalpsi(r)-getbhpsi(r))
        TotPot%fr(1,i) = -sqrt(4.*pi)*(gettotalforce(r)-bhforce(r))
c        print*, "Initial Total Pot,F Test",i,r,dr
c     &              ,TotPot%pot(1,i),TotPot%fr(1,i)
c        print*, "Initial Psi Test",r,gettotalpsi(r),getbhpsi(r),TotPot%pot(1,i)
        do l=2,lmaxx,2
            TotPot%pot(l/2+1,i)=0
        enddo
      enddo


      return
      end subroutine
cccccccc

cccccccc
      subroutine AuxillaryIni()
      implicit none

      print*, 'Auxillary Initialize'
c           If using gas, update the scale heights
      if(GasFlag) then
        call getzgasgrid()
      endif

c       Set up the first table of energies
      call gentableE()
      print*, "Done Gen Table"
c
      if(BulgeFlag) then
        call gendfsersic()
        call gendenspsibulge()
      endif
c      print*, "Done Bulge Gen"
      if(HaloFlag) then
        call gendfnfw()
c        print*, "Done gendfnfw"
        call gendenspsihalo()
        call InitialHaloDensOut()
      endif


      print*, "Done Auxillary"
      return
      end subroutine
ccccccc


ccccccccc
      subroutine PotIterate()
      implicit none
      integer niter, lmaxstep,lmax,ntheta,lmaxold,iteroutside,iter
      real drtidal,rtidalold,tidalcheck,frac
      real a00
c
c       Initialize the loops counters
c
      niter=(2+lmaxx/2)*10
      lmaxstep=2
      lmax=0
      ntheta=lmax*10+2
      ntheta=max(10,ntheta)
c
      drtidal = 2*dr
      rtidalold = 1e10
      tidalcheck = rtidalold
      lmaxold=-2
      iteroutside = 0
      frac=0.75
      MaxIterFlag=.False.
      write(*,*) 'main loop'
      do iter=0,150
c      do iter=0,0
        if(GasFlag) then        !If using a gas disk, normalize it
            call GetGasNorm(Gas)
c            print*, "Gas Norm"
        endif
        if( lmax .eq. 0 .or. lmax .eq. lmaxx ) then
            if(drtidal .lt. dr .and. iter.gt.10) then
                lmax=lmax+lmaxstep
                ntheta=lmax*4+2
            endif
        else
            lmax=lmax+lmaxstep
            ntheta=lmax*4+2
        endif
        if( lmax .eq. lmaxx+lmaxstep ) goto 199  !If lmax is larger than maximum, end the main loop

        call GetNewDensities(lmax,ntheta)      !Recalculate the density arrays
c        print*, "Quick Dens Test",TotPot%dens(0:lmaxx,1)
        call GetNewPot(lmax,lmaxold,frac)       !Get the new potential arrays
c        print*, "NewPot",HaloFlag,BulgeFlag
        if((HaloFlag .eqv. .False.)
     &          .and. (BulgeFlag .eqv. .False.)) goto 199
c        print*, "Past Flag Check"
c  finally reset the potential at the origin to psi0
        if(iter .eq. 0) call InitialPotDensOut()
        call ResetPot(a00)
        call CheckTidalRad(a00,iteroutside,iter,lmax
     &              ,drtidal,tidalcheck,rtidalold)     !Check the tidal radius

      lmaxold=lmax

      if(GasFlag) then
        call getzgasgrid()
        call GalNormDeAllocate(Gas)
      endif
      enddo
      print*, "Reached iteration limit",iter
      MaxIterFlag=.True.
 199  continue

      return
      end subroutine

cccccccc

cccccc
      subroutine GetNewPot(lmax,lmaxold,frac)
      implicit none
      integer lmax,lmaxold
      real frac

      call SimpsonHarmInt_HighL(nr,lmax,dr,lmaxold,frac
     &          ,TotPot%Dens,TotPot%Pot,TotPot%Fr,TotPot%Fr2
     &          ,TotPot%lmax)

      return
      end subroutine
ccccccccc
ccccccccc
      subroutine ResetPot(a00)
      implicit none
      real a00
      integer ir

      a00=TotPot%Pot(1,0) + getbhpsi(0.0)*sqrt(4.*pi)
c      a00=TotPot%Pot(1,0)
c      print*, "Resetting", a00,TotPot%Pot(1,0),DF%psi0
c     &              ,getbhpsi(0.0)*sqrt(4.*pi)
      do ir=0,nr
        TotPot%Pot(1,ir)=TotPot%pot(1,ir)+DF%psi0*sqrt(4.*pi)-a00
      enddo

      return
      end subroutine
ccccccccc

cccccccccc
      subroutine CheckTidalRad(a00,iteroutside,iter,lmax,drtidal
     &                  ,tidalcheck,rtidalold)
      implicit none
      real a00,drtidal,tidalcheck
      integer iteroutside,iter,lmax
      real potr,potrm1,aa,bb,dpot
      real rtidal,rtidalold
      integer i

c      print*, "Checking Tidal Rad",iter,rtidalold,DF%psi0,DF%psic,a00

      if (a00/sqrt(4.*pi)-DF%psi0.gt.DF%psic) then
        write(*,'(''Iter'',i4,'': lmax='',i4,
     &          '', tidal radius is infinite'')') iter, lmax
c        print*, a00,DF%psi0,DF%psic,a00/sqrt(4.*pi)-DF%psi0
        iteroutside = iteroutside + 1
        if( iteroutside .gt. 40 ) then
            write(*,'(''nr='',i4,'' is too small'',
     &          '', try larger number of radial bins
     &          - exiting program'')') nr
c            goto 12345
            stop
        endif
        drtidal = 2.0*dr
      else
        potr=DF%psi0
        do i=1,nr
c     look for new tidal radius of the model defined as the radius at
c     the equator where the potential is equal to psic
            potrm1=potr
            potr=pot(real(i)*dr,0.)
            aa=potrm1-DF%psic
            bb=potr-DF%psic
            if(aa*bb .le. 0.) then
c                print*, " Tidal Checks", real(i)*dr,aa,bb,potrm1,potr,DF%psic
                dpot = potr - potrm1
                if( dpot .eq. 0.0 ) then
                    rtidal = (real(i)-1)*dr
                else
                    rtidal=(real(i)-1-aa/dpot)*dr
                endif

                drtidal = abs(rtidal - rtidalold)
                tidalcheck = abs(rtidal - rtidalold)/rtidal
                write(*,'(''Iter'',i4,'': lmax='',i4,
     &              '', tidal radius is '',g15.6)') iter,lmax,rtidal
                rtidalold = rtidal
                goto 9
            endif
        enddo
        write(*,'(''Iter'',i4,'': lmax='',i4,
     &          '', tidal radius is outside grid'')') iter,lmax
        drtidal = 2.0*dr
9      endif
c      print*, "hmmmm", rtidal

      return
      end subroutine
cccccccc


ccccccc
      subroutine InitialHaloDensOut()
      implicit none
      integer i
      real r,hdns,ehdns,psi,z,psiz0
      z=0
      open(10,file='halodensity.out',status='replace')
      do i=1, nr
        if(i.eq.1) psiz0 = psi
        r=i*dr
        psi=pot(r,z)
        hdns=halodenspsi(psi)
        ehdns=halodensity(r)
c        write(10,*) r,log10(hdns),log10(ehdns),psi-psiz0
        write(10,*) r,hdns,ehdns,psi-psiz0
      enddo
      close(10)
      return
      end subroutine
cccccccc


ccccccc
      subroutine InitialPotDensOut()
      implicit none
      integer i
      real r,hdns,ehdns,psi,z,psiz0
      z=0
      open(10,file='IniPotDens.out',status='replace')
      do i=1, nr
        r=i*dr
        write(10,*) r,TotPot%pot(1,i), TotPot%Dens(1,i)
      enddo
      close(10)
      return
      end subroutine
cccccccc





      end module

