
      module DecomposeMatrix
      contains

ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     gauusj
c
c     Linear Equation solution by Gauss-Jordan elimination from
c     Press et al.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine gaussj(a,n,np,b,m,mp)
c
      implicit none
c
      integer m, mp, n, np, NMAX
      real a(np,np), b(np,mp)
      parameter(NMAX=50)

      integer i, icol, irow, j,k,l,ll,indxc(NMAX),indxr(NMAX)
      integer ipiv(NMAX)
      real big, dum, pivinv
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do 11 j=1, n
         ipiv(j)=0
 11   enddo
      do 22 i=1, n
         big=0.
         do 13 j=1, n
            if(ipiv(j) .ne. 1) then
               do 12 k=1, n
                  if(ipiv(k) .eq. 0) then
                     if(abs(a(j,k)) .ge. big) then
                        big=abs(a(j,k))
                        irow=j
                        icol=k
                     endif
                  endif
 12            enddo
            endif
 13      enddo
         ipiv(icol)=ipiv(icol)+1
         if(irow .ne. icol) then
            do 14 l=1,n
               dum=a(irow,l)
               a(irow,l)=a(icol,l)
               a(icol,l)=dum
 14         enddo
            do 15 l=1,m
               dum=b(irow,l)
               b(irow,l)=b(icol,l)
               b(icol,l)=dum
 15         enddo
         endif
         indxr(i)=irow
         indxc(i)=icol
         if(a(icol,icol) .eq. 0.)then
            print*, 'singular matrix in gaussj'
            stop
         endif
         pivinv=1./a(icol,icol)
         a(icol,icol)=1.
         do 16 l=1, n
            a(icol,l)=a(icol,l)*pivinv
 16      enddo
         do 17 l=1, n
            b(icol,l)=b(icol,l)*pivinv
 17      enddo
         do 21 ll=1,n
            if(ll .ne. icol) then
               dum=a(ll,icol)
               a(ll, icol)=0.
               do 18 l=1,n
                  a(ll,l)=a(ll,l)-a(icol,l)*dum
 18            enddo
               do 19 l=1,m
                  b(ll,l)=b(ll,l) -b(icol,l)*dum
 19            enddo
            endif
 21      enddo
 22   enddo

      do 24 l=n,1,-1
         if(indxr(l) .ne. indxc(l)) then
            do 23 k=1,n
               dum=a(k,indxr(l))
               a(k,indxr(l))=a(k,indxc(l))
               a(k,indxc(l))=dum
 23         enddo
         endif
 24   enddo
      return
      end subroutine
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     ludcmp
c
c     This subroutine decomposes a nXn matrix into LU and, in
c     combination with lubksb, solves a set of linear equations or
c     inverts the matrix
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine ludcmp(a,n,np,indx,d)
c
      implicit none
c
      integer n,np, indx(n), NMAX
      real d, a(np,np), TINY
      parameter(NMAX=500,TINY=1.0e-20)
c
      integer i,imax,j,k
      real aamax,dum,sum,vv(NMAX)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c
      d=1.
      do 12 i=1,n
         aamax=0.
         do 11 j=1,n
            if (abs(a(i,j)) .gt. aamax) aamax=abs(a(i,j))
 11      enddo
         if(aamax .eq. 0.)then
            print*, 'singular matrix in ludcmp'
            stop
         endif
         vv(i)=1./aamax
 12   enddo

      do 19 j=1,n
         do 14 i=1,j-1
            sum=a(i,j)
            do 13 k=1, i-1
               sum=sum-a(i,k)*a(k,j)
 13         enddo
            a(i,j)=sum
 14      enddo
         aamax=0.
         do 16 i=j,n
            sum=a(i,j)
            do 15 k=1, j-1
               sum=sum-a(i,k)*a(k,j)
 15         enddo
            a(i,j)=sum
            dum=vv(i)*abs(sum)
            if(dum .ge. aamax) then
               imax=i
               aamax=dum
            endif
 16      enddo
         if( j .ne. imax)then 
            do 17 k=1, n
               dum=a(imax,k)
               a(imax,k)=a(j,k)
               a(j,k)=dum
 17         enddo
            d=-d
            vv(imax)=vv(j)
         endif
         indx(j)=imax
         if(a(j,j) .eq. 0.) a(j,j)=TINY
         if (j .ne. n) then
            dum=1./a(j,j)
            do 18 i=j+1, n
               a(i,j)=a(i,j)*dum
 18         enddo
         endif
 19   enddo     

      return
      end subroutine
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     lukbksb
c
c     This solves the set of n linear equations A X=B using the
c     LU decomposition from ludcmp.  This is from Press et al.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
      subroutine lubksb(a,n,np,indx,b)
c
      implicit none
c
      integer n,np,indx(n)
      real a(np,np),b(n)
c
      integer i,ii, j, ll
      real sum
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      ii=0
      do 12 i=1,n
         ll=indx(i)
         sum=b(ll)
         b(ll)=b(i)
         if (ii .ne. 0) then
            do 11 j=ii, i-1
               sum=sum-a(i,j)*b(j)
 11         enddo
         elseif (sum .ne. 0.) then
            ii=i
         endif
         b(i)=sum
 12   enddo
      do 14 i=n,1,-1
         sum=b(i)
         do 13 j=i+1,n
            sum=sum-a(i,j)*b(j)
 13      enddo
         b(i)=sum/a(i,i)
 14   enddo
      return
      end subroutine
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      end module
