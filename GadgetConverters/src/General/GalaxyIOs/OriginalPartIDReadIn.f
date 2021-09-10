cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains the input routines for
c       the Galaxy Combine program
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module ParticleComponentIDInMod
      use ParticleCompReadInObj
      implicit none




      contains


cccccc
      subroutine PartIDReadIn(infile,P)
      implicit none
      character(*) infile
      Type(PartInObj) P
      integer i
cccc

      open(10,file=infile,status='old')
      read(10,*)
      read(10,*) P%IDTypeSwitch

      if(P%IDTypeSwitch .eq. 1) then                  !Select by Component ID
        read(10,*)
        read(10,*) P%nComp
        call AllocateComps(P)

        read(10,*)
        do i=1, P%nComp
            read(10,*) P%CompID(i,1),P%CompID(i,2),P%MLComp(i)
        enddo
      elseif(P%IDTypeSwitch .eq. 2) then              !Select by particle type
        read(10,*)
        read(10,*) P%nTypes
        call AllocateTypes(P)

        read(10,*)
        do i=1, P%nTypes
            read(10,*) P%TargPTypes(i),P%MLType(i)
        enddo
      endif


      close(10)
      return
      end subroutine
cccccc


ccccc
      subroutine ParticleComponentIDIn(InFile,P)
      implicit none
      Type(PartInObj) P
      integer i,imax
      parameter(imax=1e6)
      character(*) InFile

      print*, "Reading in Original Particle IDs from ",trim(InFile)
      open(10,file=Infile,status='old')
      read(10,*)
      do i=1,imax
        read(10,*,end=99)
      enddo
99    continue
      P%CompTot=i-1
      call AllocateTotals(P)
      rewind(10)
      read(10,*)
      do i=1,P%CompTot
        read(10,*) P%CompIDT(i,1:2),P%PTypeT(i),P%PLimsT(i,1:2)
      enddo
      close(10)


      return
      end subroutine
ccccccc

cccccccc
      subroutine AssignByCompID(P)
      implicit none
      Type(PartInObj) P

      integer i,j

      print*, "Setting up the particle assignments by Component ID"
c     &              ,P%nComp

      do i=1,P%nComp
c        print*, "hmmm", i,CID(i,1:2)
        do j=1,P%CompTot
c            print*, "Double hmmm", j,PLimsT(j,1:2)
            if(P%CompIDT(j,1) .eq. P%CompID(i,1) .and.
     &              P%CompIDT(j,2) .eq. P%CompID(i,2))then
                P%PLims(i,1)=P%PLimsT(j,1)
                P%PLims(i,2)=P%PLimsT(j,2)
c                print*, "Assign PLims Test", i,j,P%CompID(i,1:2)
c     &                      ,P%PLims(i,1:2)
            endif
        enddo
      enddo

      return
      end subroutine
cccccccccc


ccccccc
      subroutine AssignByPartType(P)
      implicit none
      Type(PartInObj) P
      integer i,j,Count


      print*, "Setting up the particle assignments by Type",P%nTypes
c       Figure out the number of components with the target type
      P%nComp=0
      do i=1,P%nTypes
        do j=1,P%CompTot
            if(P%PTypeT(j) .eq. P%TargPTypes(i))then
                P%nComp=P%nComp+1
            endif
        enddo
      enddo
      print*, "Number of components", P%nComp
      ALLOCATE(P%PLims(P%nComp,2))
      ALLOCATE(P%MLComp(P%nComp))

      Count=0
      do i=1,P%nTypes
        do j=1,P%CompTot
            if(P%PTypeT(j) .eq. P%TargPTypes(i))then
                Count=Count+1
                P%PLims(Count,1)=P%PLimsT(j,1)
                P%PLims(Count,2)=P%PLimsT(j,2)
                P%MLComp(Count)=P%MLType(i)
c                print*, "Hmmmm", i,j,Count,PLims(Count,1:2)
c     &                  ,PTypeT(j),TargPTypes(i)
            endif
        enddo
      enddo

      return
      end subroutine

      end module
