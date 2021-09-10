ccccccccccccccccccccccccccccccc
c
c     This module contains routine for converting 
c     Cartesian coordinates to Angular coordinates
c
cccccccccccccccccccccccccccccccc

      module CartAngleConvert
      use CommonConsts
      use FullCircAngs
      contains

ccccccccccccccccccccccccccccccccccccccccccc
c
c     Central Angular Coordinates to
c     Cartesian Coordinates
c
c     This subroutine converts some angular
c     coordinates (d,theta,phi) to Cartesian
c     coordinates, where theta increases CCW
c     from the X-axis and -90<Phi<90 with
c     phi=0 being in the X-Y plane.
c
c     The angles should be given in radians
c
ccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine AngToCart(Ang,Pos)
c
      implicit none
c
      real Pos(3), Ang(3)
      real rp
c
ccccccccccccccccccccccccccccccccccccccccccccc
c
      Pos(3)=Ang(1)*sin(Ang(3))
      rp=Ang(1)*cos(Ang(3))

      Pos(1)=rp*cos(Ang(2))
      Pos(2)=rp*sin(Ang(2))
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccc
c
c     Cartesian to Central Angular Coordinates
c
c     This subroutine takes Cartesian coordinates
c     and converts them to angular coordinates (d,theta,phi)
c     where theta increases CCW
c     from the X-axis and -90<Phi<90 with
c     phi=0 being in the X-Y plane.
c
c     The angles will be returned in radians
c
ccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CartToAng(Pos,Ang)
c
      implicit none
c
      real Pos(3), Ang(3)
c
ccccccccccccccccccccccccccccccccccccccccc
c
      Ang(1)=sqrt(Pos(1)**2.+Pos(2)**2.+Pos(3)**2.)
      Ang(3)=asin(Pos(3)/Ang(1))

      Ang(2)=atan_FC(Pos(1),Pos(2))

c      print*, 'CToA', Pos, Ang
      if(Ang(3) .gt. Pi/2. .or. Ang(3) .lt. -Pi/2.) then
         print*,'big prob', Pos,Ang
         stop
      endif

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccccc
c
c     Cartesian To Angular Velocity
c
c     This subroutine takes an angular position
c     and Cartesian velocities and converts
c     them to radial velocity and
c     tangential velocities in the 
c     angular directions in velocity units
c
ccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CartToAngVel(Ang,Vel,AngV)
c
      implicit none
c
      real Ang(3), Vel(3), AngV(3)
c
      
cccccccccccccccccccccccccccccccccccccccccc

      AngV(2)=-Vel(1)*sin(Ang(2))+Vel(2)*cos(Ang(2))
      AngV(1)=(Vel(1)*cos(Ang(2))+Vel(2)*sin(Ang(2)))*cos(Ang(3))
     &     +Vel(3)*sin(Ang(3))
      AngV(3)=-(Vel(1)*cos(Ang(2))+Vel(2)*sin(Ang(2)))*sin(Ang(3))
     &     +Vel(3)*cos(Ang(3))

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccc
c
c     Angular Velocity To Cartesian Velocities
c
c     This routine converts angular velocities
c     in velocity units to cartesian velocities
c
cccccccccccccccccccccccccccccccccccccccccc
c
      subroutine AngVelToCart(Ang,AngV,Vel)
c
      implicit none
c
      real Ang(3), Vel(3), AngV(3)
      real VP_3
ccccccccccccccccccccccccccccccccccccccccccc

      VP_3=AngV(1)*cos(Ang(3))-AngV(3)*sin(Ang(3))
      Vel(1)=VP_3*cos(Ang(2))-AngV(2)*sin(Ang(2))
      Vel(2)=VP_3*sin(Ang(2))+AngV(2)*cos(Ang(2))
      Vel(3)=AngV(1)*sin(Ang(3))+AngV(3)*cos(Ang(3))

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccc
c
c     Angular Velocity to Proper motions
c
c     This routine converts the angular velocity
c     in the angular directions (not radial) to proper
c     motions
c
c     It take Velocities in (100 km/s) units
c     It returns the proper motions in milli-arcseconds/yr
c
cccccccccccccccccccccccccccccccccccc
c
      subroutine AngVelToPropMot(Prop,Ang,AngV)
c
      implicit none
c
      integer i
c
      real AngV(3), Prop(2), Ang(3)
c
ccccccccccccccccccccccccccccccccccccc
c
      do i=1, 2
c     convert to parsecs per year
         Prop(i)=AngV(i+1)*100.*365.24*86400/3.0857e13
c     convert to radians per year
         Prop(i)=Prop(i)/Ang(1)/1000.
c     convert to milli-arcseconds/yr
         Prop(i)=Prop(i)*1000.*3600./Pi*180.
      enddo
      Prop(1)=Prop(1)/cos(Ang(3))  !adjust for spherical geometry
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccc
c
c     Proper motions to Angular Velocity
c
c     This routine converts the proper motion
c     in the angular directions (not radial) to 
c     angular velocities

c     It takes the proper motions in milli-arcseconds/yr
c     It returns the angular velocities in velocity units
c     (100 km/s)
c
cccccccccccccccccccccccccccccccccccc
c
      subroutine PropMotToAngVel(AngV,Ang,Prop)
c
      implicit none
c
      integer i
c
      real AngV(3), Prop(2), Ang(3)
      real tProp

c
ccccccccccccccccccccccccccccccccccccc
c

      do i=2, 3
         tProp=Prop(i-1)
         if(i .eq. 2) then
            tProp=tProp*cos(Ang(3))  !adjust for spherical geometry
         endif
c     convert to radians/yr
         tProp=tProp/(1000.*3600./Pi*180.)
c     convert to parsecs per year
         tProp=tProp*Ang(1)*1000.
c     convert to (100 km/s)
         AngV(i)=tProp/(100.*365.24*86400/3.0857e13)
      enddo
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccc




      end module
