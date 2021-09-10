cccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     CenterOfMassMod
c
c     This modules contains routines that calculate the 
c     center of mass of an n-body system and its components
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      module CentOfMassMod
c
      implicit none

      contains
ccccccccccccccccccccccccccccccccccccccccccccccccc
c     Center of Mass
c
c     This routine calculates the center of mass of some
c     particles
cccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine CenterOfMass(nPart,Pos,Mass,COM)
c
      implicit none
c
      integer nPart
      real Pos(3,nPart),COM(3),Mass(nPart)
      real MT
c
      integer i,j
cccccccccccccccccccccccccccccccccccccccccccccc

      COM(1:3)=0.
      MT=0.
      do i=1, nPart
c        print*, i, Pos(1:3,i)
         do j=1,3
            COM(j)=COM(j)+Pos(j,i)*Mass(i)
         enddo
         MT=MT+Mass(i)
      enddo

c      print*, "COM Mid Calc", COM
      do j=1,3
         COM(j)=COM(j)/MT
      enddo
c      print*, "COM Calc", COM

      return
      end subroutine
ccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccc
c
c     Iterated Center of Mass/Vel Adjust
c
c     This routine calculates the center of mass using 
c     iterated cuts by half
c
cccccccccccccccccccccccccccccccccccccccc
c
      subroutine IterCentMassVelAdjust(nPart,Pos,Vel, Mass,nIter)
      use sort
      use RecenterMod
c
      implicit none
c
      integer nPart,nIter
      real Pos(3,nPart),Vel(3,nPart),Mass(nPart)

      integer i,j,k

      real COM(3),COV(3)
      real R2(nPart)
      integer RIndex(nPart),nT
      real PT(3,nPart),MT(nPart)
c
ccccccccccccccccccccccccccccccccccccccccccccccc
c     Get the initial center of mass
      call CenterOfMass(nPart,Pos,Mass,COM) 
c     Adjust all positions
      call ShiftArray(nPart,Pos,COM)
c     Get the radius of all particles
      do i=1,nPart
         R2(i)=Pos(1,i)**2.+Pos(2,i)**2.+Pos(3,i)**2.
      enddo
c     Sort the array
      call indexx(nPart,R2,RIndex)
c      print*, COM


      nT=nPart
      do j=2,nIter
         nT=int(nPart/j)                   !Choose a subset of particles
         do i=1,nT
            k=RIndex(i)                 !Select on the inner subset
            PT(1:3,i)=Pos(1:3,k)
            MT(i)=Mass(k)
         enddo
         call CenterOfMass(nT,PT,MT(1:nT),COM) !Get the subset's center of mass
c         print*, COM
         call ShiftArray(nPart,Pos,COM)               !Adjust all positions
         do i=1,nPart                      !Re-get the various radii
            R2(i)=Pos(1,i)**2.+Pos(2,i)**2.+Pos(3,i)**2.
         enddo
         call indexx(nPart,R2,RIndex)      !And resort everything
c         print*, nT,COM
      enddo


c     Select the innermost particles
      do i=1,nT
         k=RIndex(i)           
         PT(1:3,i)=Vel(1:3,k)          !This time select the velocitys
         MT(i)=Mass(k)
      enddo
      call CenterOfMass(nT,PT,MT,COV) !Center the velocity field
      call ShiftArray(nPart,Vel,COV) !Adjust all particle velocities

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccc
c
c     Iterated Center of Mass/Vel Adjust
c
c     This routine calculates the center of mass using
c     iterated cuts by half
c
cccccccccccccccccccccccccccccccccccccccc
c
      subroutine IterCentMass(nPart,Pos,Vel, Mass,nIter
     &              ,COMTot,COVTot)
      use sort
      use RecenterMod
c
      implicit none
c
      integer nPart,nIter
      real Pos(3,nPart),Vel(3,nPart),Mass(nPart)

      integer i,j,k

      real COM(3), COMTot(3),COVTot(3)
      real R2(nPart)
      integer RIndex(nPart),nT
      real PT(3,nPart),MT(nPart)
c
cccccccccccccccccccccccccccccccccccccccccccccccc
      print*, "In Iter center of mass",nIter,nPart
c     Get the initial center of mass
      call CenterOfMass(nPart,Pos,Mass,COM)
c      print*, "COM", COM
c     Adjust all positions
      call ShiftArray(nPart,Pos,COM)
c     Get the radius of all particles
      do i=1,nPart
        R2(i)=Pos(1,i)**2.+Pos(2,i)**2.+Pos(3,i)**2.
      enddo
c     Sort the array
      call indexx(nPart,R2,RIndex)
c      print*, COM

      COMTot=COM
c      print*, "hmmm", nIter, COM
      nT=nPart
      do j=2,nIter
        nT=int(nPart/j)                   !Choose a subset of particles
        do i=1,nT
            k=RIndex(i)                 !Select on the inner subset
            PT(1:3,i)=Pos(1:3,k)
            MT(i)=Mass(k)
        enddo
        call CenterOfMass(nT,PT,MT(1:nT),COM) !Get the subset's center of mass
        COMTot=COMTot+COM
c         print*, "Iter Cent of mass",nIter,COM,COMTot
        call ShiftArray(nPart,Pos,COM)               !Adjust all positions
        do i=1,nPart                      !Re-get the various radii
            R2(i)=Pos(1,i)**2.+Pos(2,i)**2.+Pos(3,i)**2.
        enddo
        call indexx(nPart,R2,RIndex)      !And resort everything
c         print*, nT,COM
      enddo


c     Select the innermost particles
      do i=1,nT
        k=RIndex(i)
        PT(1:3,i)=Vel(1:3,k)          !This time select the velocitys
        MT(i)=Mass(k)
      enddo
      call CenterOfMass(nT,PT,MT,COVTot) !Center the velocity field

      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccccc





      end module
