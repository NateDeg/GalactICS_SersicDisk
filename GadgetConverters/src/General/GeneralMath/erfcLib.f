
      module erfcRoutines
      use GammaFnRoutines
      contains
      


ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Error Function
c 
c     This calculates the error function from Numerical
c     Recipies
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real function erfN(x)
c
      implicit none
c
      real x
c
ccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if (x .lt. 0.) then
         erfN=-gammp(0.5,x**2)
      else
         erfN=gammp(0.5,x**2)
      endif
      return
      end function
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Complementary Error Function
c
c     This function calculates the complementary error function
c     from Numerical Recipies
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real function erfcC(x)
c
      implicit none
c
      real x
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if (x .lt. 0.) then
         erfcC=1.+gammp(0.5,x**2)
      else
         erfcC=gammq(0.5,x**2)
      endif
      return
      end function
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      end module
