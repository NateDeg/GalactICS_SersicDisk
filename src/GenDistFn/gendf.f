ccccccccccc
c
c     Generate Distribute Functions
c
c       This module contains routines that calculate distribution functions
c
cccccccccc

      module DistDFMod
      use Globs
      use GetPsiMod

      implicit none


      contains
cccc

      subroutine gentableE()
      implicit none
      real r, rmin
      integer i

      print*, "Generate table of energies"

      r = 0.
      rmin = 0.001
c           Get psi at the three key radii (0, outer, minimum) -- located in CalcPsi/getpsi.f
      print*, "GentableE check",Halo%chalo,5.*Halo%drtrunchalo
      DF%psi0 = gettotalpsi(r)
      DF%psic = gettotalpsi(Halo%chalo + 5.*Halo%drtrunchalo)
      DF%psid = gettotalpsi(rmin)

      write(*,*) 'inside gentableE psi0,psid='
     &      ,DF%psi0,DF%psid,DF%psic,DF%npsi

      do i=1,npsi
         DF%tableE(i) = float(i-1)/float(DF%npsi-1)*
     &        log((DF%psi0-DF%psic)/(DF%psi0-DF%psid))
cc        print*, 'hmmm',i,psi0,psic,psid,DF%tableE(i)
         DF%tableE(i) = DF%psi0 - exp(DF%tableE(i))*(DF%psi0-DF%psid)
cc        print*, "Gen Table", i, DF%tableE(i)
      enddo
      print*, "Table E Checks", DF%psi0,DF%psic
     &          ,DF%tableE(1),DF%tableE(npsi)
     &          ,DF%psic-DF%tableE(npsi)

      return
      end subroutine
ccccccccc

      end module
