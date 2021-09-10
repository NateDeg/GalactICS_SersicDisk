cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the various input files needed
c       to generate the disk
c       (in.halo, dbh.dat, mr.dat,zgasgrid.dat,denspsihalo.dat,
c       and dfnfw.dat)
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module inGenDiskMod
      use Globs
      use GenCompGlobs
      use ReadHarmMod
      use ReadDiskDFMod
      use ReadZGasGridMod

      implicit none

      contains

      subroutine InGenDisk()
      implicit none
      character(30) dbhfile,zGridfile
      integer timearray(3)

      print*, "Gen Disk Inputs"
      read(*,*) DiskSwitch
      read(*,*) nPart
      read(*,*) Seed
      read(*,*) icofm

      idum=Seed
      if(Seed .ge. 0) then
        print*, "using time to generate random seed"
        call itime(timeArray)
        idum=abs(timeArray(1)*idum)
        idum=-int(idum*timeArray(2))-timeArray(3)
      endif

      print*, "Disk Inputs", nPart,Seed,icofm
      dbhfile='dbh.dat'

      call ReadHarmFile(dbhfile)
      call readdiskdf()


      print*, "Done Readin"

      if(GasFlag) then
        zGridfile='zgasgrid.dat'
        call ReadZGasGrid(zGridfile,Gas)
      endif


      return
      end subroutine
ccccccc

      end module

