cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module ConvertersRTInputsMod
      use ConvertersGlobs
      use CommonConsts

      implicit none

      contains

ccccc
      subroutine ConverterInput()
      implicit none
      character(100) infile,GenCompString
      integer i,j


      call getarg(1,infile)         !Get the input file containing the info needed for GalCombine
      if(infile .eq. " ") then
        print*, "Galaxy Combine Infile is required"
        stop
      endif
      open(10,file=infile,status='old')
      read(10,*) numGals,nCompMax            !Read in the number of galaxies
      print*, "Number of galaxies to combine", numGals

c       ALLOCATE the various globals
      ALLOCATE(nComponents(numGals))
      ALLOCATE(CompFileName(numGals,nCompMax))
      ALLOCATE(CenteredGalOutfiles(numGals))
      ALLOCATE(PartType(numGals,nCompMax))
      ALLOCATE(EulerAngs(3,numGals-1))
      ALLOCATE(COM(3,numGals-1))
      ALLOCATE(COV(3,numGals-1))

c           Read in the different files
      read(10,*)
      read(10,'(A)') GalactICSPath      !First get the general path of the GalactICS files
      print*, "The path to the GalactICS data files is "
     &                  ,trim(GalactICSPath)

      do i=1, numGals           !Loop through each galaxy
        read(10,*)
        read(10,*) nComponents(i)
        print*, "number of Components in",i, "Galaxy",nComponents(i)
        do j=1, nComponents(i)
            read(10,'(A)') GenCompString        !Read in the filename and particle type
            print*, GenCompString
            call SplitGalInfileString(GenCompString
     &                  ,CompFileName(i,j),PartType(i,j))       !Separate the two
            print*, "Component File and Particle Type: "
     &                  ,trim(CompFileName(i,j)),PartType(i,j)
        enddo
        read(10,'(A)') CenteredGalOutfiles(i)           !Name the file to hold each individual galaxy
        print*, "Output centered Galaxy",i, "to "
     &              ,trim(CenteredGalOutfiles(i))
      enddo

c           Get the shifts for all galaxies beyond galaxy 2
      do i=1,numGals-1
        read(10,*)
        read(10,*) EulerAngs(1:3,i)
        EulerAngs(1:3,i)=EulerAngs(1:3,i)*DegToRad
        read(10,*) COM(1:3,i)
        read(10,*) COV(1:3,i)
      enddo

c       Get the name of the final combined file
      read(10,*)
      read(10,'(A)') CombineOutFile
      print*, "Output ascii combined galaxy to ", trim(CombineOutFile)
      read(10,*)
      read(10,'(A)') CompSeparateFile   !Get the name of a file to know which particles belong to which thing
      print*, "Output Particle ID file for later separation to "
     &              ,trim(CompSeparateFile)

c       Read in the various flags for the Gadget files
      read(10,*) CombinedGal%Time
      read(10,*) CombinedGal%Redshift
      read(10,*) CombinedGal%SFR
      read(10,*) CombinedGal%Feedback
      read(10,*) CombinedGal%Cooling
      read(10,*) CombinedGal%Box
      read(10,*) CombinedGal%Omega0
      read(10,*) CombinedGal%OmegaLambda
      read(10,*) CombinedGal%Hubble

      close(10)



      return
      end subroutine
ccccccc

      subroutine SplitGalInfileString(GString,fname,iType)
      implicit none

      character(*) fname
      character(*) GString
      integer itype
      character(1) deLim
      character(100) Temp
      integer SLen,stat
c
      deLim=','
      SLen=SCAN(GString,deLim)
      Temp=GString(1:SLen-1)
      fname=trim(GalactICSPath)//trim(Temp)
c      print*, trim(fname),len(GString)
      Temp=GString(SLen+1:len(GString))
c      print*, trim(Temp)
      read(Temp,*,iostat=stat) iType
c      print*, "PartType Trial", iType

      return
      end subroutine


      end module

