cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module AllGadgetGalOutMod
      use ConvertersGlobs
      use GadgetOutMod


      implicit none

      contains


ccccc
      subroutine FinalGadgetASCIIGalOutput()
      implicit none
      integer i

      print*, "writing out final combined galaxy"
c       Start by Allocating enough 'Galaxy Obj' components
      call WriteGadgetAsciiFile(CombineOutFile,CombinedGal)


      return
      end subroutine
ccccccc

ccccc
      subroutine GadgetCompIDsOutput()
      implicit none
      integer i,j,Tot,Count

      open(10,file=CompSeparateFile,status='replace')
      write(10,*) 'Gal   Comp   PType Start   Fin'

      Tot=sum(nComponents(1:numGals))
      do Count=1,Tot
        i=PartTypeOrder(Count,1)
        j=PartTypeOrder(Count,2)
        write(10,*) i,j,PartType(i,j)
     &              ,ComponentParticleOrder(Count,1:2)

      enddo

      close(10)


      return
      end subroutine
ccccccc


      end module

