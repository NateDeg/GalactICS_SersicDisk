cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the in.dbh file used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module outDBHMod
      use Globs
      use GasNormMod
      use HaloPotMod
      use BulgePotMod
      use DiskDensGridMod
      use DensPotForceOutMod
      use GasZGridMod

      implicit none

      contains


cccccc
      subroutine OutDBH()
      implicit none

      print*, "Outputting everything"

      call MassOutputs()
      call OutputDBHFile()
      if(BHFlag) then
        call BlackHoleOut(BH%bhmass)
      endif



      return
      end subroutine


ccccccccc
      subroutine OutputDBHFile()
      implicit none
      integer ir, FUnit
      real psi

      FUnit=11

      if(gasFlag) then
        call ZGridOut(Gas)
      endif

      if(haloFlag) then         !If there is a halo make an h.dat file
        open(FUnit,file='h.dat',status='unknown')
        call OutDPFFileHeader(FUnit,1)          !Output Header
        call OutDensPotForce(FUnit,HaloPot)    !Output Dens,Potential, and Force arrays
        close(FUnit)
        write(*,*) 'Halo harmonics written to file ''h.dat''.'
      endif

      if(BulgeFlag) then         !If there is a bulge make a b.dat file
        open(FUnit,file='b.dat',status='unknown')
        call OutDPFFileHeader(FUnit,2)          !Output Header
        call OutDensPotForce(FUnit,BulgePot)    !Output Dens,Potential, and Force arrays
        close(FUnit)
        write(*,*) 'Bulge harmonics written to file ''b.dat''.'
      endif

      if( bhflag) call BlackholeOut(BH%bhmass)

      open(FUnit,file='dbh.dat',status='unknown')
      call OutDPFFileHeader(FUnit,0)          !Output Header
      call OutDensPotForce(FUnit,TotPot)    !Output Dens,Potential, and Force arrays
c           For the dbh file add in a disk density section
      write(FUnit,*)
      do ir=0,nr
            psi=pot(ir*dr,0.)
            write(FUnit,'(3g16.8)') ir*dr,diskdens(ir*dr,0.,psi,D1),
     &        diskdens(ir*dr,0.,psi,D2)
        enddo
      close(FUnit)
      write(*,*) 'Final model written to file ''dbh.dat''.'


      return
      end subroutine
ccccc
      subroutine MassOutputs()
      implicit none

      integer lmax
      real bulgemass,halomass,diskmass,totalmass
      real bulgeedge,haloedge,diskedge


      if(gasflag) then
        if(MaxIterFlag .eqv. .False.) then
            call GalNormDeAllocate(Gas)
            call getgasnorm(Gas)
        else
            call getzgasgrid()
            call getgasnorm(Gas)
        endif
      endif

      lmax = lmaxx

      halomass = 0.0
      bulgemass = 0.0
      haloedge = 0
      bulgeedge = 0
      totalmass = TotPot%fr(1,nr)/sqrt(4*pi)*(dr*nr)**2
      write(*,*) 'Total mass=', totalmass

      if( haloflag) then
        call halopotential(halomass, haloedge,lmax)  !Calculate the Halo potential and mass
      endif
      if( bulgeflag) then
        call bulgepotential(bulgemass, bulgeedge,lmax)
      endif
      if(DiskFlag1 .or. DiskFlag2) then
        diskmass=totalmass-halomass-bulgemass
        diskedge=max(D1%outdisk + 2.0*D1%drtrunc
     &              ,D2%outdisk+2.0*D2%drtrunc)
      else
        diskmass = 0.
        diskedge = 0.
      endif

      open(20,file='mr.dat',status='unknown')       !Output the component masses
      write(20,*) diskmass, diskedge
      write(20,*) bulgemass, bulgeedge
      write(20,*) halomass, haloedge
      close(20)


      return
      end subroutine
ccccccccc

cccccccc
      subroutine BlackHoleOut(bhmass)
      implicit none
      integer n
      real t,bhmass

      open(file='blackhole',unit=20,status='replace')
      n = 1
      t = 0.
      write(20,*) n,t
      write(20,*) bhmass,t,t,t,t,t,t
      close(20)
      return
      end subroutine
cccccccc

cccccccc
      subroutine ZGridOut(G)
      implicit none
      integer ir
      Type(GasObj) G
      print*, "Writing out gas z-grid to zgasgrid.dat"
      open(file='zgasgrid.dat',unit=2,status='replace')
      do ir = 0,G%nr
        write(2,*) G%rgasgrid(ir),G%zgasgrid(ir)
      enddo
      close(2)
      return
      end subroutine
ccccccc

      end module

