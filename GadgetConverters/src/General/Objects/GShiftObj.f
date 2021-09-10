cccccccccccccccccccccccccccccccccccccccccccccc
c
c     This module contains routines to calculate the moments 
c     of some a passed array of n-body particles per 
c     pixel element
c
ccccccccccccccccccccccccccccccccccccccccccccccccc

      module GShiftObjDef
      implicit none

      Type GShift
        real EulerAngs(3)
        real TargetAngCenter(3),VelocityVectorShift(3)
        real TargetPosCenter(3)
      end Type

      end module
