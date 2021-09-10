cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains writes out a GalactICS style file
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GadgetInMod
      use GalObjDef
      implicit none


      Type GalHead
        integer nPartPTypeIF(6)
        double precision MPType(6)
        double precision TSnapshot,Redshift
        integer SFRFlag,FeedbackFlag
        integer nPartT(6)
        integer CoolingFlag,nFiles
        double precision BoxSize,Omega0,OmegaLambda,HubbleParam
      end Type

      contains

ccccc
      subroutine ReadGadgetSnap(fname,G)
      implicit none
      integer funit
      character(*) fname
      Type(Galaxy) G
      Type(GalHead) GH
      integer i,PartLims(0:5,2),j
      real sum

c
      print*, "Reading Gadget File", fname
      funit=10
      open(funit,file=fname,form='unformatted',status='old')      !Open file
      call ReadGadgetHeader(funit,GH)
c      print*, "Test", GH%nPartT
      call HeaderConvert(GH,G)
c      print*, "Test2", G%nPart,G%nPartType
      call AllocateGalaxy(G)

      sum=0
      do i=0,5
        PartLims(i,1)=sum+1
        sum=sum+G%nPartType(i)
        PartLims(i,2)=sum
      enddo

      call ReadGadgetBody(funit,G)
      do i=1, G%nPart
        G%P(i)%Mass=G%P(i)%Mass*1.e10
        do j=0,5
            if(G%P(i)%ID .ge. PartLims(j,1)
     &              .and.G%P(i)%ID .le. PartLims(j,2))then
                G%P(i)%PartType=j
            endif
        enddo
c        print*, i, G%P(i)%PType,G%P(i)%ID,G%P(i)%Mass
c     &          ,G%P(i)%Pos,G%P(i)%Vel,G%P(i)%Entropy
      enddo
c      i=3
c      do i=1,5
c      print*, "First Part Check",G%P(i)%PartType,G%P(i)%ID,G%P(i)%Mass
c     &          ,G%P(i)%Pos,G%P(i)%Vel,G%P(i)%Entropy
c      enddo

      close(funit)

      return
      end subroutine
ccccccc


ccccccccccccccccccccccccccccccccccccccccccc
      subroutine ReadGadgetHeader(ReadUnit,GH)
c
      implicit none
      integer ReadUnit
      Type(GalHead) GH
c
ccccccccccccccccccccccccccccccccccccccccc
c      print*, 'Read Unit', ReadUnit
      read(ReadUnit) GH
      return
      end subroutine
cccccccccccccccccccccccccccccccccccccccccccccccccc


cccccc
      subroutine HeaderConvert(GH,G)
      implicit none
      Type(GalHead) GH
      Type(Galaxy) G

      G%nPartType(0:5)=GH%nPartT(1:6)
      G%nPart=sum(G%nPartType(0:5))

      G%Time=GH%TSnapshot
      G%Redshift=GH%Redshift
      G%Omega0=GH%Omega0
      G%OmegaLambda=GH%OmegaLambda
      G%Hubble=GH%HubbleParam
      G%Box=GH%BoxSize

      G%SFR=GH%SFRFlag
      G%Feedback=GH%FeedbackFlag
      G%Cooling=GH%CoolingFlag

      return
      end subroutine
ccccccc

ccccccc
      subroutine ReadGadgetBody(RU,G)
      implicit none
      integer RU
      Type(Galaxy) G
      real,ALLOCATABLE  ::  Temp(:,:)
      real,ALLOCATABLE :: Temp2(:)
      integer,ALLOCATABLE :: TempI(:)
      integer i


      allocate(Temp(1:3,1:G%nPart))
      allocate(Temp2(1:G%nPart))
      allocate(TempI(1:G%nPart))


      read(RU) Temp
      do i=1, G%nPart
        G%P(i)%Pos(1:3)=Temp(1:3,i)
c        if(G%P(i)%Pos(1) .le. 0.) G%P(i)%Pos(1)=-G%P(i)%Pos(1)
c        if(G%P(i)%Pos(2) .le. 0.) G%P(i)%Pos(2)=-G%P(i)%Pos(2)
c        if(G%P(i)%Pos(3) .le. 0.) G%P(i)%Pos(3)=-G%P(i)%Pos(3)
      enddo

      read(RU) Temp
      do i=1, G%nPart
        G%P(i)%Vel(1:3)=Temp(1:3,i)  
c        G%P(i)%Vel(3)=0.
      enddo

      read(RU) TempI
      do i=1, G%nPart
        G%P(i)%ID=TempI(i)
c        print*, "hmmm", i,G%P(i)%ID,TempI(i)
      enddo


      read(RU) Temp2
      do i=1, G%nPart
        G%P(i)%Mass=Temp2(i)
      enddo

      if(G%nPartType(0).eq. 0) goto 100
      read(RU) Temp2(1:G%nPartType(0))
      do i=1, G%nPartType(0)
        G%P(i)%Entropy=Temp2(i)
      enddo
100   continue

      return
      end subroutine
ccccccc


ccccc
      subroutine ReadGadgetSnap_FullAccel(fname,G)
      implicit none
      integer funit
      character(*) fname
      Type(Galaxy) G
      Type(GalHead) GH
      integer i,PartLims(0:5,2),j
      real sum

c
      print*, "Reading Gadget File with Accels", fname
      funit=10
      open(funit,file=fname,form='unformatted',status='old')      !Open file
      call ReadGadgetHeader(funit,GH)
      call HeaderConvert(GH,G)
      call AllocateGalaxy(G)

      sum=0
      do i=0,5
        PartLims(i,1)=sum+1
        sum=sum+G%nPartType(i)
        PartLims(i,2)=sum
      enddo

      call ReadGadgetBody_FullAccel(funit,G)
      do i=1, G%nPart
        G%P(i)%Mass=G%P(i)%Mass*1.e10
        do j=0,5
            if(G%P(i)%ID .ge. PartLims(j,1)
     &              .and.G%P(i)%ID .le. PartLims(j,2))then
                G%P(i)%PartType=j
            endif
        enddo
      enddo
c      i=1
c      do i=1,5
c      print*, "First Part Check",G%P(i)%PartType,G%P(i)%ID,G%P(i)%Mass
c     &          ,G%P(i)%Pos,G%P(i)%Vel,G%P(i)%Entropy
c      enddo

      close(funit)

      return
      end subroutine
ccccccc


ccccccc
      subroutine ReadGadgetBody_FullAccel(RU,G)
      implicit none
      integer RU
      Type(Galaxy) G
      real,ALLOCATABLE  ::  Temp(:,:)
      real,ALLOCATABLE :: Temp2(:)
      integer,ALLOCATABLE :: TempI(:)
      integer i


      allocate(Temp(1:3,1:G%nPart))
      allocate(Temp2(1:G%nPart))
      allocate(TempI(1:G%nPart))


      read(RU) Temp
      do i=1, G%nPart
        G%P(i)%Pos(1:3)=Temp(1:3,i)
      enddo

      read(RU) Temp
      do i=1, G%nPart
        G%P(i)%Vel(1:3)=Temp(1:3,i)
      enddo

      read(RU) TempI
      do i=1, G%nPart
        G%P(i)%ID=TempI(i)
c        print*, "hmmm", i,G%P(i)%ID,TempI(i)
      enddo


      read(RU) Temp2
      do i=1, G%nPart
        G%P(i)%Mass=Temp2(i)
      enddo

      if(G%nPartType(0).eq. 0) goto 100
      read(RU) Temp2(1:G%nPartType(0))
      do i=1, G%nPartType(0)
        G%P(i)%Entropy=Temp2(i)
      enddo



      read(RU) Temp2(1:G%nPartType(0))              ! Density
      do i=1, G%nPartType(0)
        G%P(i)%Rho=Temp2(i)
      enddo

      read(RU) Temp2(1:G%nPartType(0))              !Smoothing Lengths
      do i=1, G%nPartType(0)
        G%P(i)%Smooth=Temp2(i)
      enddo


100   continue
      read(RU) Temp2                                !Potential
      do i=1, G%nPart
        G%P(i)%Pot=Temp2(i)
      enddo

      read(RU) Temp                                 !Accelerations
      do i=1, G%nPart
        G%P(i)%Accel(1:3)=Temp(1:3,i)
      enddo

      if(G%nPartType(0).eq. 0) goto 200
      read(RU) Temp2(1:G%nPartType(0))              !dEntropy
      do i=1, G%nPartType(0)
        G%P(i)%dEntropy=Temp2(i)
      enddo
200   continue



      return
      end subroutine
ccccccc




      end module

