
      module RootfinderMod
      implicit none
      contains
      
ccccccc
      subroutine findbrackets(func,psi,rmin,rmax)
      implicit none

      real rmin,rmax,psi,psirmin,psirmax
      real func
      external func

      rmin = 1.
      rmax = 1.

77    psirmin = func(rmin)
      if(psirmin.le.psi) then
        rmin = rmin*0.1
        goto 77
      endif
88    psirmax = func(rmax)
      if(psirmax.ge.psi) then
        rmax = rmax*10.
        goto 88
      endif
      return
      end subroutine
ccccccc


ccccccc
      subroutine RTBIS (func,psi,X1,X2,XACC,XBEST)
      implicit none
C
      INTEGER    JMAX,J
      PARAMETER  (JMAX=100)
      real X1,X2,XACC,FMID,F,DX,XMID,psi,XBEST
C
      real func
      external func

      fmid = func(x2)-psi
      f = func(x1)-psi
      if(f*fmid.ge.0.0) write(*,*) psi,x2,fmid,x1,f
      IF (F*FMID.GE.0.0) then
        print*, 'Root must be bracketed for bisection.'
        stop
      endif
      IF (F.LT.0.0) THEN
        XBEST=X1
        DX=X2-X1
      ELSE
        XBEST=X2
        DX=X1-X2
      ENDIF
      DO J=1,JMAX
        DX=DX*0.5
        XMID=XBEST+DX
        fmid = func(xmid) - psi
        IF (FMID.LT.0.0) XBEST=XMID
        IF ((ABS(DX).LT.XACC).OR.(FMID.EQ.0.0)) RETURN
      ENDDO
      print*, 'too many bisections'
      END subroutine
ccccccc


      end module
