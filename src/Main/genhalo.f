ccccccc
c
c     dbh
c
c     This program generates the N-body realization of the halo
c       using the density-potential-force output from dbh
c
cccccccc
      program genhalo
      use Globs
      use inGenHaloMod
      use iniGenHaloMod
      use MakeHaloMod
      use COMAdjustMod
      use outNBodyMod
      implicit none
      character(30) fname
      integer nComIter

      nComIter=10
      print*, "Generating Halo"
      fname='halo'
      call InGenHalo()
      call IniGenHalo()
      call MakeHalo()
      if(icofm .eq. 1) call IterCOMAdjust(nComIter)
c      if(icofm .eq. 1) call RingCOMAdjust()
c      print*, "COM Sanity"
c      if(icofm .eq. 1) call RingCOMAdjust()
      call OutputNBodyFile(fname)

      return
      end



ccccccccc
