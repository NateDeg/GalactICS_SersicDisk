ccccccc
c
c     genbulge
c
c     This program generates the N-body realization of the disk
c       using the density-potential-force output from dbh
c
cccccccc
      program gendisk
      use Globs
      use GenCompGlobs
      use inGenDiskMod
      use iniGenDiskMod
      use MakeDiskMod
      use COMAdjustMod
      use outNBodyMod

      implicit none
      character(30) fname

      print*, "Generating disk"
      call InGenDisk()
      call IniGenDisk()
      call MakeDisk()
      if(DiskSwitch .eq. 1) then
        fname='disk'
      elseif(DiskSwitch.eq. 2) then
        fname='disk2'
      endif

      if(icofm .eq. 1) call COMAdjust()
      call OutputNBodyFile(fname)

      return
      end



ccccccccc
