cccccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines used for Euler 
c     rotations.  The rotation order is Z,Y,X so
c     rnew=XYZr
c
cccccccccccccccccccccccccccccccccccccccccccccccc


      module EulerRotations
      use BasicMatrixMath

      implicit none
      real :: RotAngles(3)
      real :: RotMatSave(3,3), InvRotMatSave(3,3)
!$OMP THREADPRIVATE(RotAngles,RotMatSave,InvRotMatSave)

      contains


cccccccccccccccccccccccccccccccccccccccccccc
c
c     Calculate Rotation Matrix
c
c     This subroutine calculates a rotation
c     matrix of the form X.Y.Z
c
ccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CalcRotMatrix(RotMat, alpha, beta, gamma)
c
      implicit none
c
      real RotMat(3,3)
      real alpha, beta, gamma
c
      real c1, c2, c3, s1, s2, s3
c
ccccccccccccccccccccccccccccccccccccccccc

c      print*, 'angles', alpha, beta, gamma
      c3=cos(alpha)
      c2=cos(beta)
      c1=cos(gamma)
      s3=sin(alpha)
      s2=sin(beta)
      s1=sin(gamma)
c     ZYX
      RotMat(1,1)=c3*c2
      RotMat(2,1)=c1*s3+c3*s1*s2 
      RotMat(3,1)=s1*s3-c1*c3*s2

      RotMat(1,2)=-c2*s3
      RotMat(2,2)=c1*c3-s1*s2*s3
      RotMat(3,2)=c3*s1+c1*s2*s3

      RotMat(1,3)=s2
      RotMat(2,3)=-c2*s1
      RotMat(3,3)=c1*c2

      return
      end subroutine
c
ccccccccccccccccccccccccccccccccccccccccc




cccccccccccccccccccccccccccccccccccccccccccc
c
c     Calculate Rotation Matrix
c
c     This subroutine calculates a rotation
c     matrix or the form Z.Y.X
c
ccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CalcUnRotMatrix(RotMat, alpha, beta, gamma)
c
      implicit none
c
      real RotMat(3,3)
      real alpha, beta, gamma
c
      real c1, c2, c3, s1, s2, s3
c
ccccccccccccccccccccccccccccccccccccccccc

c      print*, 'angles', alpha, beta, gamma
      c3=cos(alpha)
      c2=cos(beta)
      c1=cos(gamma)
      s3=sin(alpha)
      s2=sin(beta)
      s1=sin(gamma)

c     XYZ
      RotMat(1,1)=c1*c2
      RotMat(2,1)=c2*s1
      RotMat(3,1)=-s2

      RotMat(1,2)=c1*s2*s3-c3*s1
      RotMat(2,2)=c1*c3+s1*s2*s3
      RotMat(3,2)=c2*s3

      RotMat(1,3)=s1*s3+c1*c3*s2
      RotMat(2,3)=c3*s1*s2-c1*s3
      RotMat(3,3)=c3*c2

      return
      end subroutine
c
ccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccc
c
c     Common Rot matrix
c     
c     This calculates a rotation matrix (and inverse) to 
c     be re-used many times
c
cccccccccccccccccccccccccccccccccccc
c
      subroutine CommonRotMatCalc(Angles)
c
      implicit none
c
      real Angles(3)
c
ccccccccccccccccccccccccccccccccccccccccc

      call CalcRotMatrix(RotMatSave, Angles(1), Angles(2), Angles(3))
      call CalcUnRotMatrix(InvRotMatSave
     &     , -Angles(3), -Angles(2), -Angles(1))

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccc
c
c     Euler Rotation
c
c     This subroutine rotates a position
c     by some given Euler angles.  It assumes that 
c     the rotation matrix has already been calculated
c     by CommonRotMatCalc
c
cccccccccccccccccccccccccccccccccccccc
c
      subroutine EulerRot_CR(Pos, PosPrime)
c
      implicit none
c
      integer i
      real Pos(3), PosPrime(3)

      real PP(3,1), PPP(3,1)
      
c
cccccccccccccccccccccccccccccccccccccccc

c     Rotate the Vector
      do i=1, 3
         PP(i,1)=Pos(i)
      enddo
      call matrixMult(3,3,3,1, RotMatSave, PP, PPP)
      do i=1, 3
         PosPrime(i)= PPP(i,1)
      enddo
      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccc
c
c     Euler Rotation
c
c     This subroutine unrotates a position
c     by some given Euler angles.  It assumes that 
c     the rotation matrix has already been calculated
c     by CommonRotMatCalc
c
cccccccccccccccccccccccccccccccccccccc
c
      subroutine EulerUnRot_CR(Pos, PosPrime)
c
      implicit none
c
      integer i
      real Pos(3), PosPrime(3)

      real PP(3,1), PPP(3,1)
      
c
cccccccccccccccccccccccccccccccccccccccc

c     Rotate the Vector
      do i=1, 3
         PP(i,1)=Pos(i)
      enddo
      call matrixMult(3,3,3,1, InvRotMatSave, PP, PPP)
      do i=1, 3
         PosPrime(i)= PPP(i,1)
      enddo
      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccc
c
c     Euler Rotation
c
c     This subroutine rotates a position
c     by some given Euler angles
c
cccccccccccccccccccccccccccccccccccccc
c
      subroutine GenEulerRot(Pos, PosPrime,Ang)
c
      implicit none
c
      integer i
      real Pos(3), PosPrime(3), Ang(3)
c
      real RotMat(3,3)
      real PP(3,1), PPP(3,1)
      
c
cccccccccccccccccccccccccccccccccccccccc
c
c     get rotation matrix and it's inverse
c
c      print*, 'Ang', Ang*180./Pi
      call CalcRotMatrix(RotMat,  Ang(1), Ang(2), Ang(3))
c     Rotate the Vector
      do i=1, 3
         PP(i,1)=Pos(i)
      enddo
      call matrixMult(3,3,3,1, RotMat, PP, PPP)
      do i=1, 3
         PosPrime(i)= PPP(i,1)
      enddo
      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc




ccccccccccccccccccccccccccccccccccccccc
c
c     Euler Rotation
c
c     This subroutine rotates a position
c     by some given Euler angles
c
cccccccccccccccccccccccccccccccccccccc
c
      subroutine GenEulerUnRot(Pos, PosPrime,Ang)
c
      implicit none
c
      integer i
      real Pos(3), PosPrime(3),Ang(3)
c
      real RotMat(3,3)
      real PP(3,1), PPP(3,1)
      
c
cccccccccccccccccccccccccccccccccccccccc
c
c     get rotation matrix and it's inverse
c
      call CalcUnRotMatrix(RotMat, -Ang(3), -Ang(2), -Ang(1))
c     Rotate the Vector
      do i=1, 3
         PP(i,1)=Pos(i)
      enddo
      call matrixMult(3,3,3,1, RotMat, PP, PPP)
      do i=1, 3
         PosPrime(i)= PPP(i,1)
      enddo
      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccc
c
c     General Euler Rotation with rotation type
c
c     This subroutine rotates a position
c     using some Euler angles.
c
c     if dir=1 use GenEulerRot
c     if dir=2 use GenEulerUnRot
c
cccccccccccccccccccccccccccccccccccccc
c
      subroutine GenEulerRot_Dir(Pos, PosPrime,Ang,dir)
c
      implicit none
c
      integer dir
      real Pos(3), PosPrime(3), Ang(3)

c
cccccccccccccccccccccccccccccccccccccccc
      
      if(dir .eq. 1) then
         call GenEulerRot(Pos,PosPrime,Ang)
      else
         call GenEulerUnRot(Pos,PosPrime,Ang)
      endif

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccc
c
c     General Euler Rotation with rotation type
c
c     This subroutine unrotates a position
c     using some Euler angles.
c
c     if dir=1 use GenUnEulerRot
c     if dir=2 use GenEulerRot
c
cccccccccccccccccccccccccccccccccccccc
c
      subroutine GenEulerUnRot_Dir(Pos, PosPrime,Ang,dir)
c
      implicit none
c
      integer dir
      real Pos(3), PosPrime(3), Ang(3)

c
cccccccccccccccccccccccccccccccccccccccc
      
      if(dir .eq. 2) then
         call GenEulerRot(Pos,PosPrime,Ang)
      else
         call GenEulerUnRot(Pos,PosPrime,Ang)
      endif

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccccc
c
c     Calculate Rotation Matrix
c
c     This subroutine calculates a rotation
c     matrix of the form Z.Y.Z
c
ccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CalcRotMatrix_ZYZ(RotMat, alpha, beta, gamma)
c
      implicit none
c
      real RotMat(3,3)
      real alpha, beta, gamma
c
      real c1, c2, c3, s1, s2, s3
c
ccccccccccccccccccccccccccccccccccccccccc

c      print*, 'angles', alpha, beta, gamma
      c3=cos(alpha)
      c2=cos(beta)
      c1=cos(gamma)
      s3=sin(alpha)
      s2=sin(beta)
      s1=sin(gamma)
c     ZYX
      RotMat(1,1)=c1*c2*c3-s1*s3
      RotMat(2,1)=c1*s3+c3*s1*c2 
      RotMat(3,1)=-c3*s2

      RotMat(1,2)=-s1*c3-c1*c2*s3
      RotMat(2,2)=c1*c3-s1*c2*s3
      RotMat(3,2)=s2*s3

      RotMat(1,3)=c1*s2
      RotMat(2,3)=s2*s1
      RotMat(3,3)=c2

      return
      end subroutine
c
ccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccc
c
c     Common Rot matrix
c     
c     This calculates a rotation matrix (and inverse) to 
c     be re-used many times
c
cccccccccccccccccccccccccccccccccccc
c
      subroutine CommonRotMatCalc_ZYZ(Angles)
c
      implicit none
c
      real Angles(3)
c
ccccccccccccccccccccccccccccccccccccccccc

      call CalcRotMatrix_ZYZ(RotMatSave,
     &     Angles(1), Angles(2), Angles(3))
      call CalcRotMatrix_ZYZ(InvRotMatSave
     &     , -Angles(3), -Angles(2), -Angles(1))

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccc


      end module
