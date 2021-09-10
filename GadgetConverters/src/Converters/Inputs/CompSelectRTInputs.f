cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module CompSelectRTInputsMod
      use CompSelectGlobs
      use ParticleComponentIDInMod

      implicit none

      contains

ccccc
      subroutine CompSelectInput()
      implicit none
      character(100) infile,GenCompString
      integer i,j


      call getarg(1,infile)         !Get the input file containing the info needed for GalCombine
      if(infile .eq. " ") then
        print*, "Component Select Infile is required"
        stop
      endif
      open(10,file=infile,status='old')

      read(10,*)
      read(10,'(A)') GalInFile

      read(10,*)
      read(10,'(A)') PartIDFile
c      print*, "hmmm ", PartIDFile

      read(10,*)
      read(10,'(A)') PartSelectFile
c      print*, "Test ", trim(PartSelectFile)


      read(10,*)
      read(10,*) DownFactor
c      print*, "hmmm2", DownFactor

      read(10,*)
      read(10,*) FormatSwitch

      read(10,*)
      read(10,'(A)') CompOutFile

      close(10)

      call PartIDReadIn(trim(PartSelectFile),PartSelect)


      return
      end subroutine
ccccccc

      end module

