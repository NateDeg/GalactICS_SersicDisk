cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module reads in the various input files needed
c       to generate the halo
c       (in.halo, dbh.dat, mr.dat,zgasgrid.dat,denspsihalo.dat,
c       and dfnfw.dat)
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module inGetFreqsMod
      use Globs
      use GenCompGlobs
      use ReadHarmMod
      use ReadZGasGridMod

      implicit none

      contains

      subroutine InGetFreqs()
      implicit none
      character(30) dbhfile,zGridfile
c
      dbhfile='dbh.dat'
      call ReadHarmFileGetFreqs(dbhfile)
      nrsave=nr
      drsave=dr
      OriBulgeFlag=BulgeFlag
      OriDiskFlag1=DiskFlag1
      OriDiskFlag2=DiskFlag2
      OriHaloFlag=HaloFlag
      if(BulgeFlag) then
        dbhfile='b.dat'
        call ReadCompHarmonics(dbhfile,BulgePot)
      endif

      dbhfile='h.dat'
      call ReadCompHarmonics(dbhfile,HaloPot)
      if(GasFlag) then
        zGridfile='zgasgrid.dat'
        call ReadZGasGrid(zGridfile,Gas)
      endif

      return
      end subroutine
cccccc

cccccccc
      subroutine ReadCompHarmonics(fname,P)
      implicit none
      character(*) fname
      Type(PotObj) P
      integer funit
c
      print*, "Read Component Harmonics"
      funit=10
      open(funit,file=fname,status='old')
      call ReadHarmHeader(funit)
      call ReadHarmonics(funit,P)
      close(funit)

      return
      end subroutine
ccccccccc


      end module

