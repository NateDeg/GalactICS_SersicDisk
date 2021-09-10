cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the in.dbh file used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module DensPotForceOutMod
      use Globs
      implicit none

      contains


cccccc
      subroutine OutDPFFileHeader(FUnit,OutType)
      implicit none
      integer FUnit
      integer iFlags(6),OutType

      iFlags=0
      if(OutType .eq. 0) then
        if(DiskFlag1) iFlags(1)=1
        if(DiskFlag2) iFlags(2)=1
        if(GasFlag) iFlags(3)=1
        if(BulgeFlag) iFlags(4)=1
        if(HaloFlag) iFlags(5)=1
        if(BHFlag) iFlags(6)=1
      elseif(OutType .eq. 1) then
        iFlags(5)=1
      elseif(OutType .eq. 2) then
        iFlags(4)=1
      endif



      write(FUnit,'('' # chalo,v0,a,nnn,v0bulge,abulge,dr,nr,lmax='')')
      write(FUnit,'('' #'',7g15.5,i6,i4)') Halo%chalo,Halo%v0,Halo%a
     &      ,Bulge%nnn,Bulge%v0bulge,Bulge%abulge,
     &     dr,nr,lmaxx
      write(FUnit,'('' # psi0, haloconst, bulgeconst:'')')     !Bulge Constant is old and included for testing only
      write(FUnit,'('' #'',3g15.8)') DF%psi0,Halo%haloconst,0.
      write(FUnit,'('' # Mdisk, rdisk, zdisk, outdisk, drtrunc'')')
      write(FUnit,'('' #'',7g15.5)') D1%rmdisk, D1%rdisk
     &      ,D1%zdisk, D1%outdisk, D1%drtrunc
     &     ,D1%rhole,D1%rcore, D1%ndisk
      write(FUnit,'('' # Mdisk2, rdisk2, zdisk2, outdisk2, drtrunc2'')')
      write(FUnit,'('' #'',5g15.5)')
     &     D2%rmdisk, D2%rdisk, D2%zdisk, D2%outdisk, D2%drtrunc
     &     ,D2%rhole,D2%rcore, D2%ndisk
      write(FUnit,'('' # Mgas, rg, zg, outg, drtruncg,rzg,zgmax,gam'')')
      write(FUnit,'('' #'',8g15.5)')
     &       Gas%rmgas, Gas%rgas, Gas%outgas, Gas%zgas0
     &      ,Gas%drtruncgas,Gas%gamma
      write(FUnit,'('' # psic, psi0-psid, bhmass'')')
      write(FUnit,'('' #'',3g15.5)') DF%psic,DF%psi0-DF%psid,BH%bhmass
     &              ,BH%BHsoftening
c      write(11,'('' #'',6i5)') iFlags(1:6)    !Temporarily use integer flags for testing with old code
      write(FUnit,'('' #'',6l5)') diskflag1, diskflag2, gasflag,
     &     bulgeflag, haloflag, bhflag


      return
      end subroutine

cccccccccc

      subroutine OutDensPotForce(FUnit,P)
      implicit none
      integer FUnit
      Type(PotObj) P
      integer ir,l
ccc
      write(FUnit,'('' #  OUTPUT FROM DBH8. TOTAL POTENTIAL.'')')
      do ir=0,nr
        write(FUnit,'(8g16.8)') real(ir)*dr,(P%dens(l/2+1,ir),l=0,lmaxx,2)
      enddo
      write(FUnit,*)
      write(FUnit,*)
      do ir=0,nr
        write(FUnit,'(8g16.8)') real(ir)*dr,(P%pot(l/2+1,ir),l=0,lmaxx,2)
      enddo
      write(FUnit,*)
      write(FUnit,*)
      do ir=0,nr
        write(FUnit,'(8g16.8)') real(ir)*dr,(P%fr(l/2+1,ir),l=0,lmaxx,2)
      enddo
      write(FUnit,*)
      write(FUnit,*)
      do ir=0,nr
        write(FUnit,'(8g16.8)') real(ir)*dr,(P%fr2(l/2+1,ir),l=0,lmaxx,2)
      enddo

      return
      end subroutine

      end module

