cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module initializes the various components and
c       constants used in the diskdf program
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module iniDiskDFMod
      use GenCompGlobs
      use Globs
      use CalcPotGridMod
      use OmekapMod
      use DiskDensFMod
      use DiskSigmaMod



      implicit none

      contains

cccccccc
      subroutine IniDiskDF()
      implicit none
      real fom,fka

      print*, "Initialize diskdf"
      DF%psi00=pot(0.0,0.0)
      DUse%rfid=2.5*DUse%rdisk

      OmekapCallCount=0
      rCircCallCount=0
      call omekap(DUse%rfid,fom,fka)

      DUse%sigr = sqrt(sigr2(DUse%rfid,DUse))
      DUse%sigden = diskdensf(DUse%rfid,0.0,DUse)*2.0*DUse%zdisk

c      print*, "Testing",DUse%sigr,DUse%sigden

      DUse%sigrcrit = 3.36*DUse%sigden/fka
      DUse%qtoomre = DUse%sigr/DUse%sigrcrit
      write(0,*) 'Toomre Q = ',DUse%qtoomre, ' at R = 2.5 R_d'
      open(file='toomre2.5',status='replace',unit=11)
      write(11,*) DUse%qtoomre
      close(11)

      return
      end subroutine
ccccccc

      end module
