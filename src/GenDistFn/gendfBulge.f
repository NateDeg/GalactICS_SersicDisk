ccccccccccc
c
c     Bulge Distribution Functions
c
c       This module contains routines that calculate the bulge distribution function
c
cccccccccc

      module BulgeDFMod
      use Globs
      use GetPsiMod
      use GeneralDistDFMod


      implicit none


      contains
cccc

      subroutine gendfsersic()
      implicit none

      integer i
      real energy,dfsersiclast

      open(file='dfsersic.dat',unit=50,status='replace')
c      print*, "Checking",DF%npsi
c      do i=1,1
      do i=1,DF%npsi
c        print*, i
        energy = DF%tableE(i)
c        print*, energy
        DF%dfsersic(i) = GeneralGetDF(energy,1) !This calls the general getDF routine using
                                    ! the bulge density routines (see GenDistFn/generaldfRoutines.f)
c        print*,DF%dfsersic(i)
        if(DF%dfsersic(i).gt.0.) then
            DF%dfsersic(i) = log(DF%dfsersic(i))
            dfsersiclast = DF%dfsersic(i)
        else
            DF%dfsersic(i) = dfsersiclast
        endif
        write(50,*) DF%tableE(i),DF%dfsersic(i)
      enddo
      close(50)
      return
      end subroutine
ccccccccc


cccccc
      real function dfbulge(energy)
      implicit none
      real energy
      real rj,frac
      integer j
cccc
      if(energy.lt.DF%psic) then
        dfbulge = 0.
        return
      endif
      if(energy.ge.DF%psi0) then
        dfbulge = exp(DF%dfsersic(1))
        return
      endif

      rj = 1. + float(DF%npsi-1)*
     &      log((DF%psi0-energy)/(DF%psi0-DF%psid))/
     &      log((DF%psi0-DF%psic)/(DF%psi0-DF%psid))
      j = int(rj)
      if(j.lt.1) j = 1
      if(j.ge.DF%npsi) j = DF%npsi-1

      frac = rj - float(j)
      dfbulge = exp(DF%dfsersic(j)
     &          + frac*(DF%dfsersic(j+1)-DF%dfsersic(j)))

      return
      end function
ccccccc

      end module
