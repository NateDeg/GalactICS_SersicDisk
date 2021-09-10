ccccccc
c
c       Calculate the force using the potential grid
c
c           This module contains routines for calculating the
c       force from the galaxy at (s,z) using the potential array
c
cccccccccc

      module ForceFromGridMod
      use Globs
      use LegendreRoutines
      use AppDiskPotMod
      use AppGasPotMod
      use BHGeneralMod
      implicit none

      contains

ccccccc
      subroutine force(s,z,fs,fz,PObj)
      implicit none
      real s,z,fs,fz,gridfz
      Type(PotObj) PObj
      real fsad,fzad
      real rad,bhf

      call forceFromGrid(s,z,fs,fz,PObj)  !Get the force from the pot-Fr grid
      gridfz=fz
c      print*, "Force Test", s,z,fs,fz

      if(DiskFlag1) then
c        pot = pot + appdiskpot(s,z,D1)  !see Disk/appdiskpot.f
        call appdiskforce(s,z,fsad,fzad,D1)  !see Disk/appdiskpot.f
c        print*, "Disk1 Contribution", s,fsad,fzad
        fs = fs + fsad
        fz = fz + fzad
      endif
      if(DiskFlag2) then
c        pot = pot + appdiskpot(s,z,D2) !see Disk/appdiskpot.f
        call appdiskforce(s,z,fsad,fzad,D2) !see Disk/appdiskpot.f
c        print*, "Disk2 Contribution", fsad,fzad
        fs = fs + fsad
        fz = fz + fzad
      endif
      if(GasFlag) then
c        pot = pot + appgaspot(s,z,Gas) !see Gas/appgaspot.f
        call appgasforce(s,z,fsad,fzad,Gas) !see Gas/appgaspot.f
c        print*, "Gas Contribution", fsad,fzad
        fs = fs + fsad
        fz = fz + fzad
      endif
c      print*, "Force before BH", fs,fz
      if(BHFlag) then
        rad = sqrt(s*s + z*z)
        bhf = bhforce(rad)
        fs = fs + bhf*s/rad
        fz = fz + bhf*z/rad
      endif
c      print*, "Force after BH", fs,fz

c      print*, "Final Force", fs,fz

      return
      end subroutine
cccccccccccc


c
ccccc
      subroutine forceFromGrid(s,z,fs,fz,PObj)
      implicit none

      real s,z,fs,fz

      real pc0,pc2,pc4,pc6,pc8
      parameter (pc0=0.282094792, pc2=0.630783131, pc4=0.846284375)
      parameter (pc6=1.017107236, pc8=1.163106623)
      real pc(20), p(20), dp(20)

      real r,r1,r2,redge,t,tm1
      integer ihi,ihim1,l,i
      real costheta,ct2,sintheta,st2
      real frr,fth

      Type(PotObj) PObj

      r=sqrt(s*s+z*z)
      ihi=int(r/dr)+1
      if (ihi.lt.1) ihi=1
      if (ihi.gt.nr) ihi=nr
      r1=dr*(real(ihi)-1)
      r2=dr*real(ihi)
      redge = real(nr)*dr
      t=(r-r1)/(r2-r1)
      ihim1 = ihi - 1
      tm1 = 1.0 - t
      if (r.eq.0.) then
         fs = 0.0
         fz = 0.0
      else
         costheta=z/r
         ct2 = costheta*costheta
         sintheta=s/r
c        print*, "huh", s,z,r,costheta,ct2,sintheta
         
         do l=0,lmaxx,2
            pc(l/2+1) = sqrt((2.0*l + 1)/(4.0*pi))
            p(l/2+1) = plgndr1(l,costheta) 
            if( costheta .eq. 1.0 .or. costheta .eq. -1.0) then
               dp(l/2+1) = 0.0
            else
               st2 = 1.0 - costheta*costheta
               dp(l/2+1) = l*(plgndr1(l-1, costheta) - 
     &              costheta*p(l/2+1))/st2
c                print*, "P Test", l,p(l/2+1),dp(l/2+1)
c     &                  ,costheta,st2,plgndr1(l-1, costheta)
c     &                  ,(plgndr1(l-1, costheta)-costheta*p(l/2+1))
c     &                      /st2
            endif
         enddo
         do i=1,lmaxx/2+1
            p(i) = p(i)*pc(i)
            dp(i) = dp(i)*pc(i)
         enddo

         if( r .le. redge ) then
             frr = 0.0
             do i=1,lmaxx/2+1
                frr = frr + p(i)*(t*PObj%fr(i,ihi)
     &                  + tm1*PObj%fr(i,ihim1))
             enddo
             fth = 0.0
             do i=2,lmaxx/2+1
                fth = fth - sintheta*dp(i)*(t*PObj%pot(i,ihi) +
     &               tm1*PObj%pot(i,ihim1))
c                print*, "hmm",i,fth,sintheta,dp(i),PObj%pot(i,ihi)
c     &                  ,PObj%pot(i,ihim1)
             enddo
         else
             frr = 0.0
             do i=1,lmaxx/2+1
                 l = 2*(i-1)
                 frr = frr-(l+1)*p(i)*PObj%pot(i,nr)
     &                  /redge*(redge/r)**(l+2)
             enddo
             fth = 0.0
             do i=2,lmaxx/2+1
                 l = 2*(i-1)
                 fth = fth - sintheta*dp(i)*PObj%pot(i,nr)
     &                      *(redge/r)**(l+1)
            enddo
        endif
        fs = -(sintheta*frr + costheta/r*fth)
        fz = -(costheta*frr - sintheta/r*fth)
c        print*, "Force Grid Final", s,z,r,frr,fth,fs,fz
      endif

      return
      end subroutine


      end module

