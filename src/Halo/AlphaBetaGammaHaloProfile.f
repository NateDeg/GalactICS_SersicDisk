ccccccccccccc
c
c     Tabulated Profiles Module
c
c       This module contains routines for calculating densities and derivatives
c       for some tabulated functions
cccccccccccc


      module AlpahBetaGammaHaloMod
      use Globs
      implicit none

      contains
ccccc

ccccc
      subroutine  ABGDens(R,Dens)
c      real function TabulatedDens(R)
      implicit none
      real,Intent(IN) :: R
      real,Intent(Out) :: Dens
      real u
      real t1,t2,t3,t4

      u=R/Halo%Rs
      t1=u**Halo%gamma
      t2=1+u**Halo%alpha
      t3=(Halo%beta-Halo%gamma)/Halo%alpha
      t4=t2**t3
      Dens=Halo%rhoS/(t1*t4)


      return

      end subroutine
ccccc

ccccc
      subroutine ABGDens1Prime(R,DensPrime)
      implicit none
      real,Intent(IN) :: R
      real,Intent(Out) :: DensPrime
      real u, t1,t2,t3,t4,t5

      u=R/Halo%Rs


      call ABGDens(R,t1)
      t2=-Halo%gamma/R
      t3=((Halo%beta-Halo%gamma)/Halo%Rs)
      t4=u**(Halo%alpha-1)
      t5=1+u**(Halo%alpha)

      DensPrime=t1*(t2-t3*t4/t5)


      return
      end subroutine
cccccc


ccccc
      subroutine ABGDens2Prime(R,DensPrime2)
      implicit none
      real,Intent(IN) :: R
      real,Intent(Out) :: DensPrime2
      real u, D,D1
      real t1,t2,t3,t4,t5,t6,t7,t8


      u=R/Halo%Rs
      call ABGDens(R,D)
      call ABGDens1Prime(R,D1)

      t1=D1/D
      t2=Halo%gamma/R**2
      t3=(Halo%beta-halo%gamma)/Halo%Rs**2
      t4=u**(Halo%alpha-1.)
      t5=(Halo%alpha-1.)/u
      t6=(1.+u)**Halo%alpha
      t7=Halo%alpha*(u**(Halo%alpha-1.))
      t8=(1+u**Halo%alpha)**2.

      DensPrime2=(t1+t2-t3*t4*((t5*t6-t7)/t8))*D
      return
      end subroutine
cccccc



      end module
