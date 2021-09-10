ccccccccc
c
c     Circular Radius
c
c     This module contains routines that calculate the  radius corresponding to
c       a particular am
cccccccccc

      module RCircMod
      use OmekapMod
      use SplineMod
      use Globs
      use GenCompGlobs
      use DiskObjDef

      Type RcircObj
        real,ALLOCATABLE :: rtab(:), amtab(:), rtab2(:),rtab2zero(:)
      end Type

      Type(RcircObj) RC

      contains

      real function rcirc(am,D)
      implicit none
      real am,aam
      real om,t,slopeinf,rrc,r
      Type(DiskObj) D
      integer i
      
c make a spline fit to [Rcirc/sqrt(Lcirc)] vs. Lcirc.
c this is ~ constant at the origin, and goes like Lcirc**1.5 
c at large radii (where the potential is Kepler).

      if (RCircCallCount.eq.0) then
        ALLOCATE(RC%rtab(nr))
        ALLOCATE(RC%amtab(nr))
        ALLOCATE(RC%rtab2(nr))
        ALLOCATE(RC%rtab2zero(nr))

         do i=1,nr
            r=real((i-1))*dr
            call omekap(r,om,t)
            RC%amtab(i)=om*r*r
            RC%rtab(i)=1./sqrt(om)
         enddo
         slopeinf=1.5*RC%rtab(D%nrdisk)/RC%amtab(nr)
         call splined(RC%amtab,RC%rtab,nr,1.e32,slopeinf,RC%rtab2)
         do i=1,nr
            RC%rtab2zero(i) = 0.
         enddo         
         RCircCallCount=1
      endif
      aam=abs(am)
      if (aam.gt.RC%amtab(nr)) then
         rcirc=RC%rtab(nr)*(aam/RC%amtab(nr))**2
      else
         call splintd(RC%amtab,RC%rtab,RC%rtab2zero,nr,aam,rrc)
         rcirc=rrc*sqrt(aam)
         if(rrc.lt.0.) then
            write(*,*) 'Problem in rcirc',rrc,am
            stop
         endif
      endif

      return
      end function

      end module
