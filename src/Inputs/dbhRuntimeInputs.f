cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the in.dbh file used in GalactICS
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module inDBHMod
      use Globs

      implicit none

      contains

      subroutine InDBH()
      implicit none
      character(1) ans 

      write(*,*) 'Include halo?'
      read(*,'(a)') ans

      if(ans.eq.'y') then
        HaloFlag=.True.
        write(*,*) 'outer radius, v0, a, drtrunchalo?'
        read(*,*) Halo%chalo, Halo%v0, Halo%a
     &              ,Halo%drtrunchalo, Halo%cusp
        Halo%NumericalTableSwitch=0
      elseif(ans .eq. 'a') then
        HaloFlag=.True.
        write(*,*) "Using numerical halo"
        read(*,*) Halo%HaloProfileFile
        read(*,*) Halo%chalo, Halo%drtrunchalo
        print*, Halo%HaloProfileFile
        Halo%NumericalTableSwitch=1
      elseif(ans .eq. 'b') then
        HaloFlag=.True.
        write(*,*) 'outer radius, v0, a, drtrunchalo?'
        read(*,*) Halo%rhoS,Halo%Rs,Halo%alpha,Halo%beta,Halo%gamma
     &              ,Halo%chalo, Halo%drtrunchalo
        Halo%NumericalTableSwitch=2
      else
        HaloFlag=.False.
      endif


c Enter disk parameters
      write(*,*) 'Include disk?'
        read(*,'(a)') ans
      if( ans .eq. 'y' ) then
        DiskFlag1=.True.
        D1%DiskUseFlag=.True.
        write(*,*)
     &        'Disk mass, scale length, rad, scale height,trunc width'
        read(*,*) D1%rmdisk, D1%rdisk, D1%outdisk, D1%zdisk, D1%drtrunc,
     &        D1%rhole, D1%rcore, D1%ndisk
      else
        DiskFlag1=.False.
        D1%DiskUseFlag=.False.
        D1%rmdisk = 0.
      endif

c Enter second disk parameters
      write(*,*) 'Include disk?'
      read(*,'(a)') ans
      if( ans .eq. 'y' ) then
        DiskFlag2=.True.
        D2%DiskUseFlag=.True.
        write(*,*)
     &        'Disk mass, scale length, rad, scale height,trunc width'
        read(*,*) D2%rmdisk, D2%rdisk, D2%outdisk, D2%zdisk, D2%drtrunc,
     &        D2%rhole, D2%rcore,D2%ndisk
      else
        DiskFlag2=.False.
        D2%DiskUseFlag=.False.
        D2%rmdisk = 0.
      endif


c Enter gas disk parameters
      write(*,*) 'Include gas disk?'
      read(*,'(a)') ans
      if( ans .eq. 'y' ) then
        GasFlag=.True.
        write(*,*)
     &        'Mdisk, Rdisk, Rout, zdisk, dRout, Rzdisk, zgasmax, gamma'
        read(*,*) Gas%rmgas, Gas%rgas, Gas%outgas, Gas%GasTemp
     &      , Gas%drtruncgas, Gas%gamma
        Gas%zgas0=Gas%GasTemp
     &           *Boltzmann/massproton/(1000.**2.)/(100.**2.)
        print*, "Gas'height' ", Gas%GasTemp,Gas%zgas0
      else
        GasFlag=.False.
      endif


c Enter bulge parameters
      write(*,*) 'Include bulge?'
      read(*,'(a)') ans
      if( ans .eq. 'y' ) then
        BulgeFlag=.True.
        write(*,*) 'sersic, ppp, v0_bulge, a_bulge?'
        read(*,*) Bulge%nnn, Bulge%ppp, Bulge%v0bulge, Bulge%abulge
        else
        Bulge%v0bulge = 0.
        Bulge%abulge = 0.
        BulgeFlag = .False.
      endif

c     Enter blackhole parameters
      write(*,*) 'Include blackhole?'
      read(*,'(a)') ans
      if(ans.eq.'y') then
        write(*,*) 'blackhole mass?'
        read(*,*) BH%bhmass,BH%bhsoftening
        bhflag = .True.
      else
        BH%bhmass = 0.
        bhflag = .False.
      endif

c     Enter grid parameters
      write(*,*) 'radial step size, no. of bins?'
      read(*,*) dr,nr
      write(*,*) 'max. azimuthal harmonic l?'
      read(*,*) lmaxx

c       Get psi params
      open(file='in.gendenspsi',unit=40,status='old')
      read(40,*) npsi,nint
      close(40)


      return
      end subroutine




      end module

