      module BasicMatrixMath
      contains

cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Matirx Addition
c
c     This subroutine adds together two matrices of equal
c     dimensions
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine matrixAdd(n1, m1, n2, m2, matrix1, matrix2,result)
c
      implicit none
c
      integer n1, m1
      integer n2, m2
      integer i, j
c
      real matrix1(n1,m1)
      real matrix2(n2,m2)
      real result(n1,m1)
c
ccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if((n1 .ne. n2) .or. (m1 .ne. m2)) then
         print*, "dimensions incorrect so addition is impossible"
     &        , n1, n2, m1, m2
         stop
      endif
      
      do i=1, n1
         do j=1, m1
            result(i,j)=matrix1(i,j)+matrix2(i,j)
         enddo
      enddo
      return
      end subroutine
c
ccccccccccccccccccccccccccccccccccccccccccccccccc




cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Matirx Multiplication
c
c     This subroutine multiplies two matrices together
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine matrixMult(m1, n1, m2, n2, matrix1, matrix2,result)
c
      implicit none
c
      integer m1, n1
      integer m2, n2
      integer i, j, k
c
      real matrix1(m1,n1)
      real matrix2(m2,n2)
      real result(m1,n2)
      real temp
c
ccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(n1 .ne. m2) then
         print*, "dimensions incorrect so multiplication is impossible"
     &        , n1, n2, m1, m2
         stop
      endif
    

      result=0.
      do i=1, m1
         do j=1, n2
            do k=1, n1
               temp=matrix1(i,k)*matrix2(k,j)
               result(i,j)=temp+result(i,j)
            enddo
         enddo
      enddo

      return
      end subroutine
c
ccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Matirx Multiplication
c
c     This subroutine gives the cross product of 2 three-element vectors
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine VecCross(Vec1, Vec2,result)
c
      implicit none
c
      real Vec1(3)
      real Vec2(3)
      real result(3)
c
ccccccccccccccccccccccccccccccccccccccccccccccccc
c
      result(1)=Vec1(2)*Vec2(3)-Vec1(3)*Vec2(2)
      result(2)=Vec1(3)*Vec2(1)-Vec1(1)*Vec2(3)
      result(3)=Vec1(1)*Vec2(2)-Vec1(2)*Vec2(1)
c      print*, "Cross", Vec1,Vec2,result

      return
      end subroutine
c
ccccccccccccccccccccccccccccccccccccccccccccccccc



      end module
