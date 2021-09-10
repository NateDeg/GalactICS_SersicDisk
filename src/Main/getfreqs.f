ccccccc
c
c     get frequencies
c
c     This program calculates the frequencies used for making the disk
c
cccccccc
      program getfreqs
      use Globs
      use GenCompGlobs
      use inGetFreqsMod
      use CalcFreqsMod
      use outGetFreqsMod

      implicit none
      character(30) fname

      print*, "Generating dbh frequencies"
      fname='freqdbh.dat'
      call InGetFreqs()
      call CalcFreqs()
      call OutputFreqsFile()

      return
      end



ccccccccc
