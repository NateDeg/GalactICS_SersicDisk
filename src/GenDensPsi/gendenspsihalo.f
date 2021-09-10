ccccccccccc
c
c     Generate density-psi table
c
c       This module contains routines that calculate a table of energies and
c       associated densities
c
cccccccccc

      module HaloDensPsiMod
      use Globs
c      use GetPsiMod
      use DFObjDef
      use HaloDFMod
      use GenDensPsiMod
      implicit none


      contains
cccc
      subroutine gendenspsihalo()
      implicit none
      character(100) fname
      integer m
      m=200

      fname='denspsihalo.dat'
      call gendenspsi(fname,DF%denspsihalo(1:DF%npsi),dfhalo,m)


      return
      end subroutine
ccccccccc

cccccc
      real function halodenspsi(energy)
      implicit none
      real energy
      integer j
      real rj,frac

      if(energy.lt.DF%psic) then
        halodenspsi = 0.
        return
      endif
      if(energy.ge.DF%psi0) then
        halodenspsi = DF%denspsihalo(1)
        return
      endif

      rj = 1. + float(DF%npsi-1)*
     &      log((DF%psi0-energy)/(DF%psi0-DF%psid))
     &      /log((DF%psi0-DF%psic)/(DF%psi0-DF%psid))

c      print*, "psi values", DF%psi0,DF%psid,DF%psic

      if(isnan(rj)) then
        write(*,*) 'psi0-energy',DF%psi0-energy
        write(*,*) 'psi0-psid',dF%psi0-DF%psid
        write(*,*) 'psi0-psic',DF%psi0-DF%psic
        stop
      endif

      j = int(rj)
      if(j.lt.1) j = 1
      if(j.ge.npsi) j = npsi-1

      frac = rj - float(j)
      halodenspsi = DF%denspsihalo(j) +
     &          frac*(DF%denspsihalo(j+1)-DF%denspsihalo(j))

c      print*, "Halodenspsi",energy,rj,j,frac
c     &                  ,DF%denspsihalo(j),DF%denspsihalo(j+1)
c     &          ,halodenspsi,DF%npsi

      return
      end function
cccccccc


      end module
