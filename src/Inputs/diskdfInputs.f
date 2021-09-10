cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the various input files needed
c       to generate the diskdf
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module inDiskDFMod
      use Globs
      use GenCompGlobs
      use ReadHarmMod
      use ReadZGasGridMod

      implicit none

      contains

      subroutine InDiskDF()
      implicit none
      character(30) dbhfile,zGridfile
      integer DiskInt
c
      dbhfile='dbh.dat'
      call ReadHarmFile(dbhfile)

      write(*,*) 'Which disk to calculate'
      read(*,*) DiskInt
      if(DiskInt .eq. 1) then
        print*, "Calculating 1st disk"
        DUse=D1
        OutputFileName='cordbh.dat'
      elseif(DiskInt .eq. 2) then
        print*, "Calculating 2nd disk"
        DUse=D2
        OutputFileName='cordbh2.dat'
      endif

      write(0,*) 'Central velocity dispersion, scale length of sigR**2?'
c      read(*,*) DUse%sigr0,DUse%disksr, DUse%sig_n
c      print*, "Quick Test",DUse%disksr,DUse%sig_n
      read(*,*) DUse%sigr01,DUse%disksr1,DUse%sigr02,DUse%disksr2
      print*, "Quick Test",DUse%disksr1,DUse%sigr02,DUse%disksr2
      write(0,*) 'number of radial steps for correction fns (min. 6)?'
      read(*,*) DUse%nrspl
      write(0,*) 'number of iterations?'
      read(*,*) niter


      if(GasFlag) then
        zGridfile='zgasgrid.dat'
        call ReadZGasGrid(zGridfile,Gas)
      endif

      return
      end subroutine
cccccc

      end module

