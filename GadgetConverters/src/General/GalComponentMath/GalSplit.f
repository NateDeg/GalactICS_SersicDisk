cccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     This module contains routines for combining or
c       separating different galaxy components
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      module GalCompSplitMod
      use GalObjDef
      implicit none

      contains

ccccc
      subroutine GalCompSelect(GIni,GFin,nSplits,PartLims,ML)
      implicit none
      integer nSplits,PartLims(nSplits,2)
      Type(Galaxy) GFin,GIni
      integer i,j,k
      integer PartPerSplits(nSplits)
      real ML(nSplits)

c
      print*, "Selecting Particular Components", GIni%nPartType
c      print*, "hmmm",GIni%nPart

c      print*, nSplits
      do i=1, nSplits
        PartPerSplits(i)=PartLims(i,2)-PartLims(i,1)+1
c        print*, i,PartLims(i,1:2),PartPerSplits(i)
      enddo
      GFin%nPart=sum(PartPerSplits(1:nSplits))
c      print*, "Check", GFin%nPart
      call AllocateGalaxy(GFin)


      GFin%nPartType=0
      j=0
      do i=1,GIni%nPart
        do k=1,nSplits
            if(GIni%P(i)%ID .ge. PartLims(k,1) .and.
     &              GIni%P(i)%ID .le. PartLims(k,2))   then
                j=j+1
                GFin%P(j)=GIni%P(i)
                GFin%nPartType(GFin%P(j)%PartType)=
     &                  GFin%nPartType(GFin%P(j)%PartType)+1
                GFin%P(j)%ML=ML(k)
                GFin%P(j)%Luminosity=GFin%P(j)%Mass/ML(k)
c                print*, "Sanity Check", j,GFin%P(j)%ID
c     &                      ,GFin%P(j)%PartType
c     &                      ,GFin%P(j)%Pos
            endif
        enddo
      enddo
c      print*, "Assign Check", j,GFin%nPartType(0:5)

      GFin%Time=GIni%Time
      GFin%Redshift=GIni%Redshift
      GFin%SFR=GIni%SFR
      GFin%Feedback=GIni%Feedback
      GFin%cooling=GIni%cooling
      GFin%Box=GIni%Box
      GFin%Omega0=GIni%Omega0
      GFin%OmegaLambda=GIni%OmegaLambda
      GFin%Hubble=GIni%Hubble


      return
      end subroutine
ccccccc

cccccccccc
      subroutine GalDownSample(G,DownFactor)
      implicit none
      integer DownFactor
      Type(Galaxy) G,GTemp
      integer i,j

      j=0
      print*, "Down Sample Galaxy by a factor of ", DownFactor
      GTemp%nPart=int(G%nPart/DownFactor)
      GTemp%nPartType=int(G%nPartType/DownFactor)
      call AllocateGalaxy(GTemp)
      do i=1,G%nPart,DownFactor
        j=j+1
        GTemp%P(j)=G%P(i)
        GTemp%P(j)%Mass=GTemp%P(j)%Mass*real(DownFactor)
c        print*, "hmmm", i,j,GTemp%P(j)%PartType,G%P(i)%PartType
      enddo
      print*, "Count Test", j, GTemp%nPart
      call DeAllocateGalaxy(G)
      G=GTemp


      return
      end subroutine
cccccccccc


      end module

