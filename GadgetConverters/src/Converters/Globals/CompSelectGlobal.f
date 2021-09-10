cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains declarations for all globals
c     used in GalactICS
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module CompSelectGlobs
      use GalObjDef
      use ParticleCompReadInObj


      implicit none



      character(200) GalInFile,CompOutFile,PartIDFile
     &              ,PartSelectFile

      Type(PartInObj) PartSelect
      Type(Galaxy) IniGal,SelectedGalComp
      Type(Galaxy),ALLOCATABLE :: GalComps(:)

      integer DownFactor
      integer FormatSwitch



      end module

