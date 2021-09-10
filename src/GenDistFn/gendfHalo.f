ccccccccccc
c
c     Halo Distribution Functions
c
c       This module contains routines that calculate the halo distribution function
c
cccccccccc

      module HaloDFMod
      use Globs
      use GetPsiMod
      use GeneralDistDFMod

      implicit none


      contains
cccc

      subroutine gendfnfw()
      implicit none

      integer i
      real energy,dfnfwlast

c      print*, "In Gendfnfw"

      open(file='dfnfw.dat',unit=50,status='replace')

c      do i=1,1
      do i=1,DF%npsi
c        print*, i
        energy = DF%tableE(i)
c        print*, energy
        DF%dfnfw(i) = GeneralGetDF(energy,2)  !This calls the general getDF routine using
                                            ! the halo density routines (see GenDistFn/generaldfRoutines.f)
        if(DF%dfnfw(i).gt.0.) then
            DF%dfnfw(i) = log(DF%dfnfw(i))
            dfnfwlast = DF%dfnfw(i)
        else
            DF%dfnfw(i) = dfnfwlast
        endif
        write(50,*) DF%tableE(i),DF%dfnfw(i)
      enddo
      close(50)
      return
      end subroutine
ccccccccc

ccccccccc
      real function dfhalo(energy)
      implicit none
      real energy
      real rj,frac
      integer j

      if(energy.lt.DF%psic) then
        dfhalo = 0.
        return
      endif
      if(energy.ge.DF%psi0) then
        dfhalo = exp(DF%dfnfw(1))
        return
      endif

      rj = 1. + float(DF%npsi-1)*
     &      log((DF%psi0-energy)/(DF%psi0-DF%psid))/
     &      log((DF%psi0-DF%psic)/(DF%psi0-DF%psid))
      j = int(rj)
      if(j.lt.1) j = 1
      if(j.ge.DF%npsi) j = DF%npsi-1

      frac = rj - float(j)
      dfhalo = exp(DF%dfnfw(j) + frac*(DF%dfnfw(j+1)-DF%dfnfw(j)))
c      write(10,*) j,energy,dfnfw(j),frac,dfhalo

      return
      end function
cccccc


      end module
