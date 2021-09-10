cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module outputs the halo particles made in GenHalo.f
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module outGetFreqsMod
      use GenCompGlobs
      use Globs


      implicit none

      contains


ccccccccc
      subroutine OutputFreqsFile()
      implicit none
      character(30) fname
      integer i
      real r

      print*, "Writing out frequencies"
      fname='freqdbh.dat'
      open(10,file=trim(fname),status='replace')


      write(10,'(''#'',16(3x,a10,3x))') 'RADIUS','OMEGA_H','NU_H',
     :     'SIGMA_D','VC_TOT','VC_B','NU_B','PSIMAJ_TOT','PSI'''''
      write(10,'(''#'')')
      i=0
      write(10,'(9g16.8)')
     +     0.,HaloPot%vcmaj(1)/dr,(4*HaloPot%vertfreq(1)
     &          -HaloPot%vertfreq(2))/3.
     +          ,D1%surden(i),TotPot%vcmaj(i)
     &          ,BulgePot%vcmaj(i),
     +     (4*BulgePot%vertfreq(1)-BulgePot%vertfreq(2))/3.,
     +     TotPot%potmaj(i),
     +     TotPot%psi2(i)

      do i=1,nr
        r=real(i)*dr
        write(10,'(9g16.8)') r,HaloPot%vcmaj(i)/r
     &              ,HaloPot%vertfreq(i),D1%surden(i)
     +              ,TotPot%vcmaj(i),BulgePot%vcmaj(i)
     &              ,BulgePot%vertfreq(i),TotPot%potmaj(i)
     +              ,TotPot%psi2(i)

      enddo
      close(10)

      return
      end subroutine

ccccccc

      end module

