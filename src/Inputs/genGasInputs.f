cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the various input files needed
c       to generate the disk
c       (in.halo, dbh.dat, mr.dat,zgasgrid.dat,denspsihalo.dat,
c       and dfnfw.dat)
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module inGenGasMod
      use Globs
      use GenCompGlobs
      use ReadHarmMod
      use ReadDiskDFMod
      use ReadZGasGridMod


      implicit none

      contains

      subroutine InGenGas()
      implicit none
      character(30) dbhfile,zGridfile
      integer timearray(3)

      print*, "Gen Gas Inputs"
      read(*,*) nPart
      read(*,*) Seed
      read(*,*) icofm
      read(*,*) inFileName
      read(*,*) Gas%nrsplg
      read(*,*) Adio_IsoTempSwitch

      idum=Seed
      if(Seed .ge. 0) then
        print*, "using time to generate random seed"
        call itime(timeArray)
        idum=abs(timeArray(1)*idum)
        idum=-int(idum*timeArray(2))-timeArray(3)
      endif

c      print*, "Gas Inputs", nPart,Seed,icofm
      dbhfile='dbh.dat'
      zGridfile='zgasgrid.dat'
      call ReadHarmFile(dbhfile)
      call ReadZGasGrid(zGridfile,Gas)



c      print*, "Done Readin"


      return
      end subroutine
ccccccc

      end module

