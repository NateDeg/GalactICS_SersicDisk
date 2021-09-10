ccccccccccccccccccccccccccccccccccc
c     
c     Full Circle Angles
c
c     This routine is designed to calculated the basic inverse 
c     trig functions assuming that the angle is between 0 and 2*Pi
c     and the system is right-handed
c
c     each function assumes that you passing it X and Y
c     
ccccccccccccccccccccccccccccccccccccc


      module FullCircAngs
      use CommonConsts
      contains

cccccccccccccccccccccccccccccccccccccccc
c
c     full circle inverse sin
c      
cccccccccccccccccccccccccccccccccccccccc
c
      real function asin_FC(X,Y)
c
      implicit none
c
      real X,Y,R
cccccccccccccccccccccccccccccccccccccc
      R=sqrt(X*X+Y*Y)

      if(X .gt. 0. .and. Y .gt. 0.) then
         asin_FC=asin(Y/R)
      elseif(X .lt. 0. .and. Y .gt. 0.) then
         asin_FC=Pi+asin(Y/R)
      elseif(X .lt. 0. .and. Y .lt. 0.) then
         asin_FC=Pi+asin(Y/R)
      elseif(X .gt. 0. .and. Y .lt. 0.) then
         asin_FC=2*Pi+asin(Y/R)
      elseif(X .eq. 0.) then
         if(Y .gt. 0.) asin_FC=Pi/2.            
         if(Y .lt. 0.) asin_FC=3*Pi/2.
         if(Y .eq. 0.) asin_FC=0.         
      elseif(Y .eq. 0.) then
         if(X .gt. 0.) asin_FC=0.
         if(X .lt. 0.) asin_FC=Pi
      endif

      return
      end function
ccccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccc
c
c     full circle inverse cos
c      
cccccccccccccccccccccccccccccccccccccccc
c
      real function acos_FC(X,Y)
c
      implicit none
c
      real X,Y,R
cccccccccccccccccccccccccccccccccccccc

      R=sqrt(X*X+Y*Y)

      if(X .gt. 0. .and. Y .gt. 0.) then
         acos_FC=acos(X/R)
      elseif(X .lt. 0. .and. Y .gt. 0.) then
         acos_FC=acos(X/R)
      elseif(X .lt. 0. .and. Y .lt. 0.) then
         acos_FC=2*Pi-acos(X/R)
      elseif(X .gt. 0. .and. Y .lt. 0.) then
         acos_FC=2*Pi-acos(X/R)
      elseif(X .eq. 0.) then
         if(Y .gt. 0.) acos_FC=Pi/2.            
         if(Y .lt. 0.) acos_FC=3*Pi/2.
         if(Y .eq. 0.) acos_FC=0.         
      elseif(Y .eq. 0.) then
         if(X .gt. 0.) acos_FC=0.
         if(X .lt. 0.) acos_FC=Pi
      endif

      return
      end function
ccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccc
c
c     full circle inverse cos
c      
cccccccccccccccccccccccccccccccccccccccc
c
      real function atan_FC(X,Y)
c
      implicit none
c
      real X,Y
cccccccccccccccccccccccccccccccccccccc

      if(X .gt. 0. .and. Y .gt. 0.) then
         atan_FC=atan(Y/X)
      elseif(X .lt. 0. .and. Y .gt. 0.) then
         atan_FC=Pi+atan(Y/X)
      elseif(X .lt. 0. .and. Y .lt. 0.) then
         atan_FC=Pi+atan(Y/X)
      elseif(X .gt. 0. .and. Y .lt. 0.) then
         atan_FC=2*Pi+atan(Y/X)
      elseif(X .eq. 0.) then
         if(Y .gt. 0.) atan_FC=Pi/2.            
         if(Y .lt. 0.) atan_FC=3*Pi/2.
         if(Y .eq. 0.) atan_FC=0.         
      elseif(Y .eq. 0.) then
         if(X .gt. 0.) atan_FC=0.
         if(X .lt. 0.) atan_FC=Pi
      endif
c      print*, 'tan', X,Y,atan_FC

      return
      end function
ccccccccccccccccccccccccccccccccccccccccccccccc

      end module
