cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the various input files needed
c       to generate the halo
c       (in.halo, dbh.dat, mr.dat,zgasgrid.dat,denspsihalo.dat,
c       and dfnfw.dat)
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module inGenHaloMod
      use Globs
      use GenCompGlobs
      use ReadHarmMod
      use ReadMRMod
      use ReadDensPsiMod
      use ReadZGasGridMod

      implicit none

      contains

      subroutine InGenHalo()
      implicit none
      character(30) dbhfile,zGridfile
      integer timearray(3)

      print*, "Gen Halo Inputs"
      read(*,*) StreamingFrac
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

      print*, "Halo Inputs", StreamingFrac,nPart,Seed,icofm
      call readmassrad()
      dbhfile='dbh.dat'
      call ReadHarmFile(dbhfile)
      call ReadDensPsiHalo()
      if(GasFlag) then
        zGridfile='zgasgrid.dat'
        call ReadZGasGrid(zGridfile,Gas)
      endif
c      print*, "Done Readin"


      return
      end subroutine


      end module

