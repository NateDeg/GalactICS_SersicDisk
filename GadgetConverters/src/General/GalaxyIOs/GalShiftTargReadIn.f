cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalShiftInMod
      use GShiftObjDef
      use CommonConsts
      use SolarParams
      implicit none




      contains


cccccc
      subroutine GShiftReadIn(infile,GS)
      implicit none
      character(*) infile
      Type(GShift) GS

cccc

      open(10,file=infile,status='old')
c       Read in the target center, velocity, and Euler Angles
      read(10,*)
      read(10,*) GS%TargetAngCenter(2:3),GS%TargetAngCenter(1)
      GS%TargetAngCenter(2:3)=GS%TargetAngCenter(2:3)*DegToRad
      GS%TargetAngCenter(1)=GS%TargetAngCenter(1)*1000.

      read(10,*)
      read(10,*) GS%VelocityVectorShift(1:3)

      read(10,*)
      read(10,*) GS%EulerAngs(1:3)
      GS%EulerAngs(1:3)=GS%EulerAngs(1:3)*DegToRad

c       Read in the solar position and velocity
      read(10,*)
      read(10,*) R0

      read(10,*)
      read(10,*) VSol(1:3)
      close(10)
      return
      end subroutine
cccccc



      end module
