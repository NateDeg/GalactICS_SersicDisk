ccccccc
c
c     gengas
c
c     This program generates the N-body realization of the gas disk
c       using the density-potential-force output from dbh
c
cccccccc
      program gengas
      use Globs
      use GenCompGlobs
      use inGenGasMod
      use iniGenGasMod
      use MakeGasMod
      use COMAdjustMod
      use outNBodyMod

      implicit none
      character(30) fname

      print*, "Generating gas disk"
      call InGenGas()
      call IniGenGas()
      call MakeGas()
      if(icofm .eq. 1) call GasCOMAdjust()
      fname='gasdisk'
      call OutputNBodyFile(fname)

      return
      end



ccccccccc
