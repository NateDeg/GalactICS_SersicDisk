ccccccccccccc
c
c     NFW Profiles Module
c
c       This module contains routines the center of mass/velocity and adjusting
c       them for the array of particles found in GenCompGlobs
cccccccccccc

      module COMAdjustMod
      use GenCompGlobs
      use sort


      implicit none

      contains


ccccccc
      subroutine COMAdjust()
      implicit none
      integer i
      Type(Particle) COM

      print*, "Adjusting Center of Mass"
      call COMCalc(COM)
      print*, "COM Check", COM%Pos,COM%Vel

      do i=1, nPart
        P(i)%Pos(1:3)=P(i)%Pos(1:3)-COM%Pos(1:3)
        P(i)%Vel(1:3)=P(i)%Vel(1:3)-COM%Vel(1:3)
      enddo
      call COMCalc(COM)
      print*, "COM Check", COM%Pos,COM%Vel

      return
      end subroutine
ccccccccc

ccccccccc
      subroutine ArrayCOMCalc(nArr,PosArr,COM)
      implicit none
      integer i,nArr
      real PosArr(nArr,3)
      Type(Particle) COM

      COM%Pos=0.
      do i=1,nArr
        COM%Pos(1:3)=COM%Pos(1:3)+PosArr(i,1:3)
c        print*, "Testing", i,PosArr(i,1:3)
      enddo
      COM%Pos(1:3)=COM%Pos(1:3)/real(nArr)
c      print*, "Array COM", nArr,COM%Pos

      return
      end subroutine
ccccccccccc

      subroutine RingCOMAdjust()
      implicit none

      integer nR
      real dR
      real,ALLOCATABLE::PTemp(:,:),VTemp(:,:),R(:)
      integer,ALLOCATABLE :: RIndx(:),RID(:),nPerRing(:)
      integer i,j,PartCount,RingCount
      Type(Particle),ALLOCATABLE :: COM(:),COV(:)

      nR=20
      dR=1.

c       Allocate important arrays
      ALLOCATE(R(nPart))
      ALLOCATE(RID(nPart))
      ALLOCATE(RIndx(nPart))
      ALLOCATE(nPerRing(nPart+1))
      ALLOCATE(COM(nPart+1))
      ALLOCATE(COV(nPart+1))
      nPerRing=0

c       Do a mina COM adjustment
      call COMAdjust()
c       Get the cylindrical radius of all particles
      do i=1, nPart
        R(i)=sqrt(P(i)%Pos(1)**2.+P(i)%Pos(2)**2.)
        RID(i)=int(R(i)/dR)+1           !Place the particles in some ring
        if(RID(i) .gt. nR) then
            RID(i)=nR+1
        endif
        nPerRing(RID(i))=nPerRing(RID(i))+1
      enddo
cc       Sort by radius
      call indexx(nPart,R,RIndx)


      PartCount=0
      RingCount=1
      ALLOCATE(PTemp(nPerRing(RingCount),3))
      ALLOCATE(VTemp(nPerRing(RingCount),3))

      do i=1,nPart
        j=RIndx(i)
        if(R(j) .ge. RingCount*dR .and. RingCount .le. nR) then
c            print*,"Switching to new ring", i,j,RingCount,R(j),RingCount*dR
c     &                  ,(RingCount+1)*dR,PartCount,nPerRing(RingCount)
            RingCount=RingCount+1
            PartCount=0
            ALLOCATE(PTemp(nPerRing(RingCount),3))
            ALLOCATE(VTemp(nPerRing(RingCount),3))

        endif


        PartCount=PartCount+1
        PTemp(PartCount,1:3)=P(j)%Pos(1:3)
        VTemp(PartCount,1:3)=P(j)%Vel(1:3)

        if(PartCount.eq. nPerRing(RingCount)) then
            call ArrayCOMCalc(PartCount,PTemp,COM(RingCount))
            call ArrayCOMCalc(PartCount,VTemp,COV(RingCount))
            print*, "Ring COM Test", PartCount,COM(RingCount)%Pos(3)
     &                  , COV(RingCount)%Pos(3)
            DEALLOCATE(PTemp)
            DEALLOCATE(VTemp)
        endif

      enddo

      do i=1, nPart
        j=RID(i)
        P(i)%Pos(3)=P(i)%Pos(3)-COM(j)%Pos(3)
        P(i)%Vel(3)=P(i)%Vel(3)-COV(j)%Pos(3)
      enddo



      return
      end subroutine



ccccccc
      subroutine IterCOMAdjust(nComIter)
      implicit none
      integer nComIter,i,nTemp,j
      real,ALLOCATABLE :: TempPosT(:,:),TempPos(:,:),TempVel(:,:),R(:)
      integer,ALLOCATABLE :: RIndx(:)
      Type(Particle) COM

      ALLOCATE(TempPosT(nPart,3))
      ALLOCATE(R(nPart))
      ALLOCATE(RIndx(nPart))
      do i=1, nPart
        TempPosT(i,1:3)=P(i)%Pos(1:3)
c        print*, "Initial", P(i)%Pos(1:3)
      enddo
      call ArrayCOMCalc(nPart,TempPosT,COM)
c      print*, "All Part CoM Test", COM%Pos,nComIter

c      do j=2,2
      do j=2,nComIter
        call ArrayAdjust(nPart,TempPosT,COM%Pos)        !Adjust all positions by the last CoM
c               Calculate R for new positions
        do i=1, nPart
            R(i)=sqrt(TempPosT(i,1)**2.+TempPosT(i,2)**2.+TempPosT(i,3)**2.)
        enddo
        call indexx(nPart,R,RIndx)  !Sort by R
c           Select only some fraction of the particles
        nTemp=nPart
        ALLOCATE(TempPos(nTemp,3))
        do i=1,nTemp
            TempPos(i,1:3)=TempPosT(RIndx(i),1:3)
c            print*, "Sanity",i,RIndx(i),TempPosT(i,1:3),TempPosT(RIndx(i),1:3)
        enddo
        i=nPart/j
        call ArrayCOMCalc(nTemp,TempPos,COM)        !Calculate the CoM
        print*, "Iterated CoM", COM%Pos,nTemp, R(Rindx(i))
        DEALLOCATE(TempPos)

      enddo
      call ArrayAdjust(nPart,TempPosT,COM%Pos)  !Adjust all positions by the final CoM

c           Adjust Velocities using only the inner regions
      ALLOCATE(TempVel(nTemp,3))
      do i=1,nTemp
        TempVel(i,1:3)=P(RIndx(i))%Vel(1:3)
      enddo
      call ArrayCOMCalc(nTemp,TempVel,COM)      !Get the COV of the reduced array
      DEALLOCATE(TempVel)
      ALLOCATE(TempVel(nPart,3))
      do i=1, nPart
        TempVel(i,1:3)=P(i)%Vel(1:3)
      enddo
      call ArrayAdjust(nPart,TempVel,COM%Pos)       !Adjust all velocities by CoV

c       Finally replace the particle arrays with the new positions and velocities
      do i=1, nPart
        P(i)%Pos=TempPosT(i,1:3)
        P(i)%Vel=TempVel(i,1:3)
      enddo


      return
      end subroutine
cccccc

      subroutine ArrayAdjust(nArr,Arr,Adjust)
      implicit none
      integer nArr,i
      real Adjust(3),Arr(nArr,3)

      do i=1, nArr
c        print*, "Arr Adjust", i, nArr,Arr(i,1:3),Adjust(1:3)
        Arr(i,1:3)=Arr(i,1:3)-Adjust(1:3)
      enddo
      return
      end subroutine




ccccccc
      subroutine COMCalc(COM)
      implicit none
      integer i
      Type(Particle) COM

      COM%Pos=0.
      COM%Vel=0.
      do i=1, nPart
        COM%Pos(1:3)=COM%Pos(1:3)+P(i)%Pos(1:3)
        COM%Vel(1:3)=COM%Vel(1:3)+P(i)%Vel(1:3)
      enddo
      COM%Pos(1:3)=COM%Pos(1:3)/real(nPart)
      COM%Vel(1:3)=COM%Vel(1:3)/real(nPart)


      return
      end subroutine
ccccccccc


ccccccc
      subroutine GasCOMAdjust()
      implicit none
      integer i
      Type(Particle) COM

      print*, "Adjusting Gas Center of Mass"
      call COMCalc(COM)
c      print*, "COM Check", COM%Pos,COM%Vel

      do i=1, nPart
        P(i)%Pos(1:3)=P(i)%Pos(1:3)-COM%Pos(1:3)
        P(i)%Vel(1:2)=P(i)%Vel(1:2)-COM%Vel(1:2)
      enddo
      call COMCalc(COM)
c      print*, "COM Check", COM%Pos,COM%Vel

      return
      end subroutine
ccccccccc


      end module
