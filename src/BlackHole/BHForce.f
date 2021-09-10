cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines for calculating forces
c       due to the BH
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module BHGeneralMod
      use Globs
      implicit none

      contains

ccccccc
      real function bhforce(r)
      implicit none
      real R,ratio

      if(BHFlag) then
        ratio = r/BH%bhsoftening
        bhforce = -BH%bhmass*ratio
     &          /(BH%bhsoftening**2)/((1. + ratio**2)**1.5)
      else
         bhforce = 0.
         return
      endif

      return
      end function
ccccccccccc


ccccccccc
      real function getbhpsi(r)
      implicit none
      real r, ratio

      if(BHFlag) then
        ratio = r/BH%bhsoftening
        getbhpsi = BH%bhmass/BH%bhsoftening/((1. + ratio**2)**0.5)
      else
        getbhpsi = 0.
      endif

      return
      end function
cccccccccc

ccccccc
      real function getbhdpsi(r)
      implicit none
      real r, ratio

      if(BHflag) then
        ratio = r/BH%bhsoftening
        getbhdpsi = BH%bhmass*ratio/(BH%bhsoftening**2)/((1. + ratio**2)**1.5)
      else
        getbhdpsi = 0.
      endif

      return
      end function
cccccc

ccccc
      real function getbhd2psi(r)
      implicit none
      real r, ratio

      if(BHflag) then
        ratio = r/BH%bhsoftening
        getbhd2psi = BH%bhmass/BH%bhsoftening*
     +     (1.-ratio**2)/((1. + ratio**2)**2.5)
      else
        getbhd2psi = 0.
      endif

      return
      end function
cccccccc


      end module

