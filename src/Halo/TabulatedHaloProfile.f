ccccccccccccc
c
c     Tabulated Profiles Module
c
c       This module contains routines for calculating densities and derivatives
c       for some tabulated functions
cccccccccccc


      module TabulatedHaloMod
      use Globs
      implicit none

      contains
ccccc

ccccc
      subroutine  TabulatedDens(R,Dens)
c      real function TabulatedDens(R)
      implicit none
      real,Intent(IN) :: R
      real,Intent(Out) :: Dens
      real radlog
      integer j
      real frac

c      radlog = log(R)
      radlog = R
      call locate(Halo%RProf,Halo%nRProfile,radlog,j)
      j = max(j,1)
      j = min(j,Halo%nRProfile-1)
      frac = (radlog - Halo%RProf(j))/(Halo%RProf(j+1)-Halo%RProf(j))

c      Dens = exp(Halo%DProf(j) +
c     +     frac*(Halo%DProf(j+1) - Halo%DProf(j)))
      Dens = Halo%DProf(j) +
     +     frac*(Halo%DProf(j+1) - Halo%DProf(j))
c      if(frac .gt. 1.) Dens=Dens*exp(-(frac-1.)**2.)

c      print*, "Tabulated Dens Test", radlog, j
c     &      ,Halo%RProf(j),Halo%RProf(j+1),frac,Dens,exp(-frac)

      return
      end subroutine
ccccc

ccccc
      subroutine TabulatedDens1Prime(R,DensPrime)
      implicit none
      real,Intent(IN) :: R
      real,Intent(Out) :: DensPrime
      real radlog
      integer j
      real frac

c      radlog = log(R)
      radlog = R
      call locate(Halo%RProf,Halo%nRProfile,radlog,j)
      j = max(j,1)
      j = min(j,Halo%nRProfile-1)
      frac = (radlog - Halo%RProf(j))/(Halo%RProf(j+1)-Halo%RProf(j))
c      frac = frac/sqrt(1. + ((radlog - rtarget(j))/rmaxextr)**2.)
c      DensPrime = -exp(Halo%D1Prof(j) +
c     +     frac*(Halo%D1Prof(j+1) - Halo%D1Prof(j)))
      DensPrime = Halo%D1Prof(j) +
     +     frac*(Halo%D1Prof(j+1) - Halo%D1Prof(j))
c      if(frac .gt. 1.) DensPrime=DensPrime*exp(-(frac-1.)**2.)
      return
      end subroutine
cccccc


ccccc
      subroutine TabulatedDens2Prime(R,DensPrime2)
      implicit none
      real,Intent(IN) :: R
      real,Intent(Out) :: DensPrime2
      real radlog
      integer j
      real frac

c      radlog = log(R)
      radlog = R
      call locate(Halo%RProf,Halo%nRProfile,radlog,j)
      j = max(j,1)
      j = min(j,Halo%nRProfile-1)
      frac = (radlog - Halo%RProf(j))/(Halo%RProf(j+1)-Halo%RProf(j))
c      frac = frac/sqrt(1. + ((radlog - rtarget(j))/rmaxextr)**2.)
c      DensPrime2 = exp(Halo%D2Prof(j) +
c     +     frac*(Halo%D2Prof(j+1) - Halo%D2Prof(j)))
      DensPrime2 = Halo%D2Prof(j) +
     +     frac*(Halo%D2Prof(j+1) - Halo%D2Prof(j))
c      if(frac .gt. 1.) DensPrime2=DensPrime2*exp(-(frac-1.)**2.)
      return
      end subroutine
cccccc


ccccccccc
C
      SUBROUTINE LOCATE (XX,N,X,J)
      implicit none
C
      INTEGER  N,JL,JU,JM,J
      REAL     XX(N),X
c      integer count
C
c      print*, "Locate", N,X
c      print*, "huh", XX(1:100)
      JL=0
      JU=N+1
c      count=0
10    IF (JU-JL.GT.1) THEN
        JM=(JU+JL)/2
c        print*, count,JU,JL,JM
c      count=count+1
        IF ((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM))) THEN
          JL=JM
        ELSE
          JU=JM
        ENDIF
      GOTO 10
      ENDIF
      J=JL
      RETURN
      END SUBROUTINE
ccccccc

      end module
