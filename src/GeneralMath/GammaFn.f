
      module GammaFnRoutines
      implicit none
      contains
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     gammaln
c
c     Returns the natural logarthimic value of the gamma function
c
c     From Numerical Recipies
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real function gammln(xx)
c
      implicit none
c
      integer j
      real xx
      double precision ser, stp, tmp, x,y, cof(6)
c
      SAVE cof, stp
      Data cof, stp/76.18009172847146d0,-86.50532941677d0,
     *     24.01409824083091d0, -1.231739572450155d0, .1208650973866d-2,
     *     -.5395239384953d-5,2.5066282746310005d0/
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190014d0
      do j=1, 6
         y=y+1.d0
         ser=ser+cof(j)/y
      enddo
      gammln=real(tmp+log(stp*ser/x))

      return
      end function
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc






ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     gammp
c
c     This returns the incomplete gamma function P(a,x)
c
c     From Numerical Recipies
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      function gammp(a,x)
c
      implicit none
c
      real a, x, gammp
      real gammcf, gamser, gln
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      print*, "gammp arg check", a,x
      if( x .lt. 0. .or. a .le.0.)then
         print*, 'bad arguments in gammp'
         stop
      endif
      if(x .lt. a+1.) then
         call gser(gamser, a, x, gln)
         gammp=gamser
      else
         call gcf(gammcf, a, x, gln)
         gammp=1.-gammcf
      endif
      return
      end function
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     gammp
c
c     This returns the incomplete gamma function Q(a,x)
c
c     From Numerical Recipies
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      function gammq(a,x)
c
      implicit none
c
      real a, x, gammq
      real gammcf, gamser, gln
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if( x .lt. 0. .or. a .le.0.) then
         print*, x, a
         print*, 'bad arguments in gammq'
         stop
      endif
      if(x .lt. a+1.) then
         call gser(gamser, a, x, gln)
         gammq=1.-gamser
      else
         call gcf(gammcf, a, x, gln)
         gammq=gammcf
      endif
      return
      end function
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Gser
c
c     From Numerical Recipies
c     It's need form calculating the gamma funtion
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine gser(gamser, a, x, gln)
c
      implicit none
c
      integer ITMAX
      real a, gamser, gln, x, EPS
      parameter(ITMAX=100, EPS=3.e-7)
c
      integer n
      real ap, del, sum
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      gln=gammln(a)
      if(x .le. 0.)then
         if(x .lt. 0.)then
            print*,'x<0 in gser'
         endif
         gamser=0.
         return
      endif
      ap=a
      sum=1./a
      del=sum
      do n=1, ITMAX
         ap=ap+1
         del=del*x/ap
         sum=sum+del
         if(abs(del) .lt. abs(sum)*EPS) goto 1
      enddo
      print*, 'a too large, ITMAX too small in gser'
 1    gamser=sum*exp(-x+a*log(x)-gln)
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     GCF
c
c     This suboutine is needed for the incomplete gamma function
c
c     From Numerical Recipies
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine gcf(gammcf, a, x, gln)
c
      implicit none
c
      integer ITMAX
      real a, gammcf, gln, x, EPS, FPMIN
      parameter(ITMAX=100, EPS=3.e-7, FPMIN=1.e-30)

      integer i
      real an, b,c,d,del,h
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      gln=gammln(a)
      b=x+1.-a
      c=1./FPMIN
      d=1./b
      h=d
      do i=1, ITMAX
         an=-i*(i-a)
         b=b+2
         d=an*d+b
         if(abs(d) .lt. FPMIN) d=FPMIN
         c=b+an/c
         if(abs(c) .lt. FPMIN) c=FPMIN
         d=1./d
         del=d*c
         h=h*del
         if(abs(del-1.) .lt. EPS) goto 1
      enddo
      print*, 'a too large, ITMAX too small in gcf',a,x,gln,del
 1    gammcf=exp(-x+a*log(x)-gln)*h
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real FUNCTION GAMMQm1 (A,X)
      implicit none
C
      real X,A,GAMSER,GLN
C
      IF (X.LT.0.0.OR.A.LE.0.0)then
         print*, 'neg argument of gammqm1'
         stop
      endif
      CALL GSER (GAMSER,A,X,GLN)
      GAMMQm1=GAMSER
      RETURN
      END function
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc





      end module
