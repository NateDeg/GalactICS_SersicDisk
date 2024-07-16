cccccccccc
c
c     Get Gas normalization
c
c       This module contains routines for getting the normalization
c       of the gas disk
c
ccccccccccc
c
      module GasNormMod
      use SplineMod
      use GasObjDef
      use GasZMod
      use CalcPotGridMod
      use ForceFromGridMod

      implicit none
      contains


cccccc
      subroutine getgasnorm(G)
      implicit none
      Type(GasObj) G

      integer nz
      parameter(nz=200)
      real coefff(0:nz),psir0

      integer j,i
      real rmax,drg,r
      real zgas,dz,z
      real sum
      real psi,rho
      real y1,yn
ccccc
c      print*, "In Gas Normalization"
      open(file='in_gas.txt',unit=1,status='old')
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*) G%nrsplg
      close(1)
c      print*, "First Check",G%nrsplg
      call GalNormIni(G)

      coefff(1:nz-1) = 1
      coefff(0) = 0.5
      coefff(nz) = 0.5

      rmax = G%outgas + 4.*G%drtruncgas
      drg = rmax/float(G%nrsplg)

      do j = 0,G%nrsplg
         r = float(j)*drg
         if(r.eq.0.) r = G%eps
         zgas = getzgas(r,G)
         dz = 5.*zgas/float(nz-1)
         psir0 = pot(r,0.0)
         sum = 0.
         do i=1,nz-1
            z = float(i-1)*dz
            psi = pot(r,z)
            rho = exp((psi-psir0)/G%zgas0)
            sum = sum + coefff(i)*rho
         enddo

         G%gasnorm(j) = sum*2.*dz
         G%gasnormrad(j) = r
         if(G%gasnorm(j).eq.G%gasnorm(j)+1) then
            write(0,*) 'not-a-number in getgasnorm',j,r,G%gasnorm(j)
     &              ,dz,zgas,psir0,psi
            stop
         endif
      enddo

      y1 = 1e30
      yn = 1e30

      call splined(G%gasnormrad,G%gasnorm,G%nrsplg,y1,yn,G%gasnorm2)
      return
      end subroutine
ccccccccccccccccccccc


ccccccccc
      subroutine getdgasnorm(G,P)
      implicit none
      Type(GasObj) G
      Type(PotObj) P
      integer nz,j,i
      parameter(nz=200)
      real coef(0:nz)
      real rmax,drg,r,z,y1,yn
      real zgas,dz
      real fs,fz,psi
      real fs0,fz0,psir0
      real rho,sum


      coef = 1
      coef(0) = 0.5
      coef(nz) = 0.5

      rmax = G%outgas + 4.*G%drtruncgas
      drg = rmax/float(G%nrsplg)            !G%nrsplg must be read in earlier

      do j = 0,G%nrsplg
        r = float(j)*drg
        if(r.eq.0.) r = G%eps
        zgas = getzgas(r,G)
        dz = 5.*zgas/float(nz-1)
        call force(r,0.0,fs0,fz0,P)
        psir0=pot(r,0.0)
        sum = 0.
        do i=1,nz-1
            z = float(i-1)*dz
            call force(r,z,fs,fz,P)
            psi=pot(r,z)
            rho = (fs-fs0)/G%zgas0*exp((psi-psir0)/G%zgas0)
            sum = sum + coef(i)*rho
        enddo
        G%dgasnorm(j) = sum*2.*dz
        G%gasnormrad(j) = r
c        print*, "dGasNorm Test",j,G%gasnormrad(j),G%dgasnorm(j)
      enddo

      y1 = 1e30
      yn = 1e30

      call splined(G%gasnormrad,G%dgasnorm,G%nrsplg,y1,yn,G%dgasnorm2)


      return
      end subroutine
cccccccc


      end module

