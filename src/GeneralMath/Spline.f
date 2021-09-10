
      module SplineMod

      implicit none
      contains

cccccc
      subroutine splined(x,y,n,yp1,ypn,y2)
      implicit none

      integer n,nmax
      parameter (nmax = 100000)
      real yp1,ypn,x(n),y(n),y2(n)
      integer i,k
      real p,qn,sig,un,u(nmax)

c      print*, "In Splined", n
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     &        1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     &        u(i-1))/p
      enddo

c      print*, "splined, 1st loop done", n, i, sig
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
c      print*, "Next loop soon", qn,un,n
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
      enddo
c      print*, "Done splined"

      return
      end subroutine

ccccccc

ccccccc
      subroutine splintd(xa,ya,y2a,n,x,y)
      implicit none
      integer n
      real x,y,xa(n),y2a(n),ya(n)
      integer k,khi,klo
      real a,b,h
      data klo,khi /1,1/
      save khi,klo
c     check first of previous klo and khi are still ok.
c      print*, "spline hmmm", y2a(1)

      if (khi.le.n) then
        if (xa(khi).gt.x .and. xa(klo).lt.x .and. khi-klo.eq.1) goto 2
      endif
c     if not, search by bisection
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
            khi=k
        else
            klo=k
        endif
        goto 1
      endif
2     h=xa(khi)-xa(klo)

      if (h.le.0.) then
        write(*,*) 'bad xa input in splint'
        stop
      endif
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*
     &          (h**2)/6.
      if(isnan(y)) then
        write(0,*) 'finding a not-a-number in splintd'
        write(0,*) x,y,klo,khi,n
        write(0,*) a*ya(klo)
        write(0,*) b*ya(khi)
        write(0,*) (a**3-a)*y2a(klo)
        write(0,*) (b**3-b)*y2a(khi)*(h**2)/6.
c        print*, xa(khi),ya(khi),xa(klo),ya(klo),y2a(khi)
c        print*, "hmmm", y2a
c        write(0,*), "Thingy", a, b
        stop
      endif


      return
      END subroutine

ccccccc

cccccc
      subroutine splintdlong(xa,ya,y2a,n,x,y)
      implicit none
      integer n
      real*4 x,y,xa(n),y2a(n),ya(n)
      integer k,khi,klo
      real*4 a,b,h
      data klo,khi /1,1/
      save khi,klo
c     check first of previous klo and khi are still ok.

      if (khi.le.n) then
        if (xa(khi).gt.x .and. xa(klo).lt.x .and. khi-klo.eq.1) goto 2
      endif
c     if not, search by bisection
      klo=1
      khi=n
 1    if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
            khi=k
        else
            klo=k
        endif
        goto 1
      endif
 2    h=xa(khi)-xa(klo)

      if (h.le.0.) then
        write(*,*) 'bad xa input in splint'
        stop
      endif
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*
     &      (h**2)/6.
      write(0,*) 'inside splintdlong',y
      return
      END subroutine
cccccc



      end module
