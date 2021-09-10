
      module SimpsonIntMod
      use CommonConsts
      implicit none
      contains
cccccc
      subroutine SimpsonHarmInt(nx,dx,densArr,PotArr,ForArr)
      implicit none
      integer nx
      real densArr(0:nx),PotArr(0:nx),ForArr(0:nx)
      real dx

      integer i
      real s1a,s2a,s1(0:nx),s2(0:nx)
      real x,xold

      s1(0)=0.
      x=2.*dx
      s1(2)=(x*dx/3.)*(4*densArr(1)*(1.0-dx/x)**2.+densArr(2))
      xold=x
      do i=4,nx,2
        x=real(i)*dx
        s1a = (x*dx/3.)*(densArr(i-2)*(1.0-2*dx/x)**2+
     &          4*densArr(i-1)*(1.0-dx/x)**2+densArr(i))
        s1(i) = s1a + s1(i-2)*xold/x
        xold = x
      enddo
      s2(nx)=0
      xold = real(nx)*dx
      do i=nx-2,2,-2
        x=real(i)*dx
        s2a = (x*dx/3.)*(densArr(i+2)*(1.0+2*dx/x)+
     &        4*densArr(i+1)*(1.0+dx/x)+densArr(i))
        s2(i) = s2a + s2(i+2)
        xold = x
      enddo

      do i = 2,nx,2
        x = real(i)*dx
        potArr(i)=(4.*pi)*(s1(i)+s2(i))
        ForArr(i)=-(4.*pi)*s1(i)/x
c        print*, "Simpson Test", i,DensArr(i),potArr(i),ForArr(i)
      enddo

c      do i=2,10,2
c        print*, "Simpson Check", i,DensArr(i),potArr(i),ForArr(i)
c     &          ,s1(i),s2(i),x,dx
c      enddo

      potArr(0)=3*(PotArr(2)-PotArr(4))+PotArr(6)
      ForArr(0)=0.0

c     then linearly interpolate other bins.

      do i=1,nx-1,2
        potArr(i)=(potArr(i-1)+potArr(i+1))/2.
        ForArr(i)=(ForArr(i-1)+ForArr(i+1))/2.
      enddo

c      do i=1,nx
c        print*, "Initial Pot",i,potArr(i),ForArr(i)
c      enddo

      return
      end subroutine


ccccccc
      subroutine SimpsonHarmInt_HighL(nx,nl,dx,lmold,frac
     &              ,DensArr,PotArr,ForArr,For2Arr,lmx)
      implicit none
      integer nx,nl,lmold,lmx
      real dx,frac
      real DensArr(0:lmx,0:nx),PotArr(0:lmx,0:nx)
      real ForArr(0:lmx,0:nx),For2Arr(0:lmx,0:nx)

      integer l,i
      real s1a,s2a,s1(0:nx),s2(0:nx)
      real x,xold

c      print*, "in HighL Simpson Int", nx,nl,dx

c      do i=0,10
cc      do i=0,nx
c        l=0
cc        do l=0,nl,2
c            print*, "Sanity Checks", i,l,l/2+1,densArr(l/2+1,i)
cc        enddo
c      enddo

      do l=0,nl,2
        s1(0)=0
        x=2.*dx
        s1(2)=(x*dx/3.)*(4*densArr(l/2+1,1)*(1.0-dx/x)**2
     &              +densArr(l/2+1,2))
        xold=x
        do i=4,nx,2
            x=real(i)*dx
            s1a = (x*dx/3.)*(densArr(l/2+1,i-2)*(1.0-2*dx/x)**(l+2)+
     &          4*densArr(l/2+1,i-1)*(1.0-dx/x)**(l+2)+densArr(l/2+1,i))
            s1(i) = s1a + s1(i-2)*(xold/x)**(l+1)
            xold = x
c            print*, "hmmmm", i,l,x,s1(i),s1a,densArr(l/2+1,i-2)
c     &              ,densArr(l/2+1,i-1),densArr(l/2+1,i)
        enddo

        s2(nx)=0
        xold = real(nx)*dx
        do i=nx-2,2,-2
            x=real(i)*dx
            s2a = (x*dx/3.)*(densArr(l/2+1,i+2)*(1.0+2*dx/x)**(1-l)+
     &        4*densArr(l/2+1,i+1)*(1.0+dx/x)**(1-l)+densArr(l/2+1,i))
            s2(i) = s2a + s2(i+2)*(x/xold)**l
            xold = x
c            print*, "hmmmm2", i,l,x,s2(i),s2a,densArr(l/2+1,i+2)
c     &              ,densArr(l/2+1,i+1),densArr(l/2+1,i)
        enddo
c     replace the potential harmonics with a mean of the previous
c     iteration (25%) and the current one (75%). This damps out
c     oscillations that otherwise occur.  if this is the first time this
c     harmonic is calculated, use the entire new value.

        do i = 2,nx,2
c            print*, i,l,lmold,potArr(l/2+1,i),s1(i),s2(i),s1(i)+s2(i)
            if(l .le. lmold) then
                potArr(l/2+1,i)=frac*potArr(l/2+1,i)
     &                  +(1.-frac)*(4.*pi)/(2.*l+1)*(s1(i)+s2(i))
            else
                potArr(l/2+1,i)=(4.*pi)/(2*l+1)*(s1(i)+s2(i))
            endif
c            print*, "New Pot", PotArr(l/2+1,i)
        enddo

c     Calculate the 1st and 2nd-order radial gradients
        do i = 2,nx,2
            x=real(i)*dx
            ForArr(l/2+1,i)=-4*pi/(2.*l+1.)*(-(l+1)*s1(i) + l*s2(i))/x
            For2Arr(l/2+1,i)=-4*pi/(2.*l+1.)*
     &              ((l+1)*(l+2)*s1(i)/x**2+
     &              l*(l-1)*s2(i)/x**2 -(2*l+1)*DensArr(l/2+1,i))

        enddo
      enddo
      PotArr(1,0)=3*(PotArr(1,2)-PotArr(1,4))+PotArr(1,6)
      ForArr(1,0)=0.
      For2Arr(1,0)=2*For2Arr(1,2)-For2Arr(1,4)
      do l=2,nl,2
        PotArr(l/2+1,0)=0
        ForArr(l/2+1,0)=0
        For2Arr(l/2+1,0)=0
      enddo
c      print*, "SimpHarm_HighL 1st Pot Check",PotArr(1,0)
c     &          ,PotArr(1,2),PotArr(1,4),PotArr(1,6)

c  then linearly interpolate other bins.

      do i=1,nx-1,2
        do l=0,nl,2
            PotArr(l/2+1,i)=(PotArr(l/2+1,i-1)+PotArr(l/2+1,i+1))/2.
            ForArr(l/2+1,i)=(ForArr(l/2+1,i-1)+ForArr(l/2+1,i+1))/2.
            For2Arr(l/2+1,i)=(For2Arr(l/2+1,i-1)+For2Arr(l/2+1,i+1))/2.
        enddo
      enddo

      return
      end subroutine
cccc

cccccc
      subroutine BasicSimpsonInt(func,x0,xn,n,sum)
      implicit none
      real func
      external func
      real x,x0,xn,sum,dx
      integer n,i

c      print*, "BasicSimpson Int", x0,xn,n
      dx=(xn-x0)/real(n)
      sum=func(x0)+func(xn)+4*func(x0+dx)
c      print*, "huh",dx,sum
      do i=2,n-1,2
c        print*, "hmmm", i
        x=x0+real(i)*dx
        sum=sum+2*func(x)+4*func(x+dx)
      enddo
      sum=sum*dx/3.
      return
      end subroutine
ccccccc


cccccc
      subroutine BasicSimpsonIntExtraParam(func
     &              ,x0,xn,n,sum,Extra)
      implicit none
      real func
      external func
      real x,x0,xn,sum,dx
      real Extra
      integer n,i
c      print*, "BasicSim Extra", x0,xn,n,Extra
c      print*, "Basic Simp Extra",x0
      dx=(xn-x0)/real(n)
c      print*, "Basic Simp Extra"
      sum=func(x0,Extra)+func(xn,Extra)+4*func(x0+dx,Extra)
c      print*, "Basic Simp Extra",x0
      do i=2,n-1,2
        x=x0+real(i)*dx
        sum=sum+2*func(x,Extra)+4*func(x+dx,Extra)
      enddo
      sum=sum*dx/3.
c      print*, "Extra", sum
      return
      end subroutine
ccccccc


      end module
