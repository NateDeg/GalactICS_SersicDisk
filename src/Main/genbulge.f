ccccccc
c
c     genbulge
c
c     This program generates the N-body realization of the bulge
c       using the density-potential-force output from dbh
c
cccccccc
      program genbulge
      use Globs
      use GenCompGlobs
      use inGenBulgeMod
      use iniGenBulgeMod
      use MakeBulgeMod
      use COMAdjustMod
      use outNBodyMod

      implicit none
      character(30) fname

      print*, "Generating bulge"
      fname='bulge'
      call InGenBulge()
      call IniGenBulge()
      call MakeBulge
      if(icofm .eq. 1) call COMAdjust()
      call OutputNBodyFile(fname)

      return
      end



ccccccccc
