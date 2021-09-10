ccccccc
c
c     get frequencies
c
c     This program calculates the frequencies used for making the disk
c
cccccccc
      program diskdf
      use Globs
      use GenCompGlobs
      use inDiskDFMod
      use iniDiskDFMod
      use CalcDiskDFMod


      implicit none

      print*, "making the cordbh file(s)"
      call InDiskDF()
      call IniDiskDF()
      call CalcDiskDF()
      call OutputDiskDFCor()

      return
      end



ccccccccc
