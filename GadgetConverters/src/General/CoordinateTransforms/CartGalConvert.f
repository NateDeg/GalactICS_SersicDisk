cccccccccccccccccccccccccccccccccc
c
c     This module contains routines for converting 
c     cartesian coordinates centered on the 
c     Galactic center to Galactocentric 
c     coordinates.  It places the Sun at 
c     (R0,0,0) and uses a LHS so the solar 
c     velocity is at (u,-(w+vlsr),v)
c
cccccccccccccccccccccccccccccccccccccc


      module CartGalConvert
      use SolarParams
      use CartAngleConvert
      use RecenterMod

      contains


cccccccccccccccccccccccccccccccccccccccccc
c
c     Galactic to Cartesian
c
c     This subroutine takes Angular (d,l,b) coordinates
c     and converts them to positions 
c     about the Galactic Center
c
c     l and b are assumed to be in radians
c
c     This routine places the Sun at (R0,0,0)
c
ccccccccccccccccccccccccccccccccccccccccc
c
      subroutine GalactoToCart(Pos,Ang)
c
      implicit none
c
      real Pos(3), Ang(3)
      real TempPos(3)
c
ccccccccccccccccccccccccccccccccccccccc
      
c      print*, 'GalactoTesting', Ang
      call AngToCart(Ang,TempPos)
      Pos(3)=TempPos(3)
      Pos(2)=-TempPos(2)
      Pos(1)=-TempPos(1)+R0
c      print*, 'result',Pos
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccc
c
c     Cartesian to Galactocentric coordinates
c
c     This subroutine takes a cartesian position
c     and converts it to an angle (d,l,b)
c
c     l and b are returned in radians
c
c     This routine places the Sun at (R0,0,0)
c
cccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CartToGalacto(Ang,Pos)
c
      implicit none
c
      real Pos(3)
      real TempPos(3), Ang(3)
c
cccccccccccccccccccccccccccccccccccccccc

c      print*, "C2Gal Ini", Pos,Ang
      TempPos(3)=Pos(3)
      TempPos(1)=-Pos(1)+R0
      TempPos(2)=-Pos(2)
c      print*, 'temp',TempPos
      call CartToAng(TempPos,Ang)
c      print*, "Final Ang", Ang(1),Ang(2:3)*180./3.14

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccc
c
c     Cartesian Velocity to Observed Galactocentric velocities
c
c     This routine takes cartesian velocities and
c     converts them to velocities in l,b, and r
c
c     Make sure any adjustments for the solar motion
c     have already been done
c
cccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CartVelToGalacto(Ang,Vel,AngV)
c
      implicit none
c
      integer i
      real Ang(3), Vel(3), AngV(3)
      real VelP(3)
c
cccccccccccccccccccccccccccccccccccccc
      do i=1, 3
         VelP(i)=Vel(i)
      enddo
c
c     Switch the velocities to realign for l,b
c
c      print*, 'cvg',VelP,Ang

      VelP(1)=-VelP(1)
      VelP(2)=-VelP(2)
      call CartToAngVel(Ang,VelP,AngV)
c      print*, "Hmmmm", VelP,AngV

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccc



ccccccccccccccccccccccccccccccccccccccccc
c
c     Cartesian Velocity to Observed Galactocentric velocities
c
c     This routine takes cartesian velocities and
c     converts them to velocities in l,b, and r
c
c     Make sure any adjustments for the solar motion
c     have already been done
c
cccccccccccccccccccccccccccccccccccccccccc
c
      subroutine GalactoToCartVel(Ang,AngV,Vel)
c
      implicit none
c
      integer i
      real Ang(3), Vel(3), AngV(3)
      real VelP(3)
      real vl,vb,vR,lp,bp
c
cccccccccccccccccccccccccccccccccccccc
      do i=1, 3
         VelP(i)=AngV(i)
      enddo
c
c     Switch the velocities to realign for l,b
c

      vR=AngV(1)
      vl=AngV(2)
      vb=AngV(3)
      lp=Ang(2)
      bp=Ang(3)

c      print*, 'galtocartvel', vr,vl,vb,lp,bp

      Vel(1)=-vl*cos(lp)+vb*sin(bp)*sin(lp)-vR*cos(bp)*sin(lp)
      Vel(2)=-vl*sin(lp)-vb*sin(bp)*cos(lp)+vR*cos(bp)*cos(lp)
      Vel(3)=+vb*cos(bp)+vR*sin(bp)      

      vR=Vel(1)
      Vel(1)=-Vel(2)
      Vel(2)=vR
c      print*, 'res',Vel(1:3)

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     GSR To HelioCentric Radial Velocity
c
c     This routine converts a galactic standard of rest
c     radial velocity to an observed heliocentric radial velocity
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine GSRtoHelRadVel(HelVel,AngPos,GSRV)
      
      use SolarParams
c
      implicit none
c
      real HelVel,AngPos(2),GSRV
      real d, Pos(3), AngV(3),VelP(3)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      d=100
      Pos(1)=d
      Pos(2)=AngPos(1)
      Pos(3)=AngPos(2)
      AngV(1)=GSRV
      AngV(2)=0.
      AngV(3)=0.

      call GalactoToCartVel(Pos,AngV,VelP)
      call recenter(VelP,VSol)
      call CartVelToGalacto(Pos,VelP,AngV)
      HelVel=AngV(1)
c      print*, 'GSRV', GSRV,HelVel



      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccc
     



ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     HelioCentric Radial To GSR Velocity
c
c     This routine converts a galactic standard of rest
c     radial velocity to an observed heliocentric radial velocity
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine HeltoGSRRadVel(HelVel,AngPos,GSRV)
      use SolarParams
c
      implicit none
c
      real HelVel,AngPos(2),GSRV
      real d, Pos(3), AngV(3),VelP(3)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c


      d=100
      Pos(1)=d
      Pos(2)=AngPos(1)
      Pos(3)=AngPos(2)
c      AngV(1)=GSRV
      AngV(1)=HelVel
      AngV(2)=0.
      AngV(3)=0.

      call GalactoToCartVel(Pos,AngV,VelP)
      call recenter(VelP,-VSol)
      call CartVelToGalacto(Pos,VelP,AngV)
      GSRV=AngV(1)
c      print*, 'GSRV', GSRV,HelVel



      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccc
     

      end module
