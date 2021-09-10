cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains writes out a GalactICS style file
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GadgetOutMod
      use GalObjDef
      implicit none

      contains

ccccc
      subroutine WriteGadgetAsciiFile(fname,G)
      implicit none
      integer funit
      character(*) fname
      Type(Galaxy) G
c
      funit=10
      open(funit,file=fname,status='replace')      !Open file
      call WriteGadgetHeader(funit,G)
      call WriteGadgetMain(funit,G)
      close(funit)

      return
      end subroutine
ccccccc


ccccccccc
      subroutine WriteGadgetHeader(funit,G)
      implicit none
      integer funit
      Type(Galaxy) G

      print*, "Writing Gadget Header", funit
      write(funit,*) G%nPartType        !The number of particles in each type
      write(funit,*) 0,0,0,0,0,0        !Give all mass array values as 0 so that Gadget reads specific masses
      write(funit,*) G%Time                 !Set the initial time to 0
      write(funit,*) G%Redshift                 !Set the initial
      write(funit,*) G%SFR
      write(funit,*) G%Feedback
      write(funit,*) G%nPartType
      write(funit,*) G%cooling
      write(funit,*) 1                  !The number of files is set to 1 in this code
      write(funit,*) G%Box
      write(funit,*) G%Omega0
      write(funit,*) G%OmegaLambda
      write(funit,*) G%Hubble

      return
      end subroutine
ccccccc


ccccccccc
      subroutine WriteGadgetMain(funit,G)
      implicit none
      integer funit
      Type(Galaxy) G
      integer i

      do i=1, G%nPart
        write(10,*) G%P(i)%ID, G%P(i)%PartType
     &             ,G%P(i)%Mass
     &             ,G%P(i)%Pos,G%P(i)%Vel          
     &             ,G%P(i)%Entropy, G%P(i)%Temp,G%P(i)%Rho,G%P(i)%NE

c        print*, "Testing Entropy",G%P(i)%Rho,G%P(i)%Entropy
      enddo
      return
      end subroutine
ccccccc

      end module

