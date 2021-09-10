ccccccccccc
c
c     fna mid density
c
ccccccccc


      module fnamiddenMod
      use DiskObjDef
      use SplineMod
      use DiskDensFMod
      implicit none

      contains

      real function fnamidden(r,D)
      implicit none
      real r,fcor
      Type(DiskObj) D

      
      call splintd(D%rr(0:D%nrspl),D%fdrat(0:D%nrspl)
     &          ,D%drat2(0:D%nrspl),D%nrspl+1,r,fcor)
      fnamidden=diskdensf(r,0.0,D)*fcor

      return
      end function


      end module
