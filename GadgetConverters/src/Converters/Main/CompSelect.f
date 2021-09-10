ccccccc
c
c     Component Selector
c
c     This program reads in a Gadget output and selects a
c       particular component(s) and outputs them as Gadget files
c
c       It also allows for down-sampling to be done
c
cccccccc
      program CompSelect
      use GadgetInMod
      use CompSelectRTInputsMod
      use ParticleComponentIDInMod
      use GalCompSplitMod
      use GadgetOutMod


      implicit none

      print*, "Component Selector"
      call CompSelectInput()
      call ReadGadgetSnap(GalInFile,IniGal)
      call ParticleComponentIDIn(PartIDFile,PartSelect)
      if(PartSelect%IDTypeSwitch .eq. 1) then
        call AssignByCompID(PartSelect)
      elseif(PartSelect%IDTypeSwitch .eq. 2) then
        call AssignByPartType(PartSelect)
      endif
      call GalCompSelect(IniGal,SelectedGalComp,PartSelect%nComp
     &          ,PartSelect%PLims,PartSelect%MLComp)
      call GalDownSample(SelectedGalComp,DownFactor)
      if(FormatSwitch .eq. 1) then
        call WriteGadgetAsciiFile(CompOutFile,SelectedGalComp)
      endif

      return
      end



ccccccccc
