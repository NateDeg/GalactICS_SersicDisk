ccccccc
c
c     Galaxy Combine
c
c     This program combines the outputs of two GalactICS galaxies after
c       rotating and shifting the second galaxy
c
cccccccc
      program CombineGal
      use ConvertersRTInputsMod
      use GalactICSCompInMod
      use ConvertersGalCompCombine
      use AllGalactICSGalOutMod
      use MoveGalsMod
      use GalCompAddMod
      use AllGadgetGalOutMod
      use GalUnitsMod

      implicit none
      real MConv,VConv
      MConv=2.325e9/1.e10
      VConv=100.

      print*, "Combining Galaxies"

      call ConverterInput()             ! Get the main inputs
      call AllGalactICSCompsReadIn()    !Read in the various galaxies
      call MoveAllGals()                !Move each galaxy except the 1st galaxy
      call AllGalCombine()              !Combine the components into each galaxy
      call AllGalactICSGalOutput()      !Output each individual galaxy
      call CombineFinalGal()            !Combine Each galaxy together
      call GalMassConv(CombinedGal,MConv)         !Convert to Gadget Masses
      call GalVelConv(CombinedGal,VConv)          !Convert To Gadget Velocities
c      call GalGasTypeConvert(CombinedGal)
c      call FinalGalactICSGalOutput()    !Output the final combined galaxy
      call FinalGadgetASCIIGalOutput()    !Output the final combined galaxy
      call GadgetCompIDsOutput()

      return
      end



ccccccccc

cccccc
      subroutine GalGasTypeConvert(G)
      use ConvertersRTInputsMod
      implicit none
      integer i
      Type(Galaxy) G

      print*, G%nPartType(0:5)
      G%nPartType(1)=G%nPartType(1)+G%nPartType(0)
      G%nPartType(0)=0
      print*, G%nPartType(0:5)

      do i=1, G%nPartType(1)
        G%P(i)%PartType=1
      enddo


      return
      end subroutine
cccccccc

