ccccccccccccccccccccccccccccccccccccccc
c
c     Set Nan
c
c     This module contains a function designed to return a NaN
c
ccccccccccccccccccccccccccccccccccccccc

      module SetNanMod
     
      contains
      real function SetNan()
c
      implicit none
      real NanT
      NanT=-1.
      SetNan=sqrt(NanT)
      return
      end function
cccccccccccccccccccccccccccccccccccc

cccccc
      real function minvalNan(Arr,ASize)
      implicit none
      integer ASize
      real Arr(ASize)
      integer i

      minvalNan=1.e20
      do i=1,ASize
        if(Arr(i) .ne. Arr(i)+1) then
            if(Arr(i) .lt. minvalNan) then
                minvalNan=Arr(i)
            endif
c        else
c            Arr(i)=0.
        endif
      enddo
c      print*, "Arr Size", size(Arr)
c      print*, 'minvalNan',minvalNan, minval(Arr)
      return
      end function
cccc

cccccc
      real function maxvalNan(Arr,ASize)
      implicit none
      integer ASize
      real Arr(ASize)
      integer i

      maxvalNan=-1.e20
      do i=1,ASize
        if(Arr(i) .ne. Arr(i)+1) then
            if(Arr(i) .gt. maxvalNan) then
                maxvalNan=Arr(i)
            endif
        endif
      enddo
c      print*, "Arr Size", size(Arr)
c      print*, 'minvalNan',minvalNan, minval(Arr)
      return
      end function
cccc

      end module
