
      module LegendreRoutines
      contains
      


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      real FUNCTION PLGNDR1(L,X)
      implicit none
      real X
      integer L,LL
      real PMM,PMMP1,PLL
ccccccc
c      print*, "plgndr1", L,X 

      PMM=1.
      IF(L.EQ.0) THEN
        PLGNDR1=PMM
      ELSE
        PMMP1=X*PMM
        IF(L.EQ.1) THEN
            PLGNDR1=PMMP1
        ELSE
            DO 12 LL=2,L
                PLL=(X*(2*LL-1)*PMMP1-(LL-1)*PMM)/(LL)
                PMM=PMMP1
            PMMP1=PLL
12        CONTINUE
        PLGNDR1=PLL
        ENDIF
      ENDIF
      RETURN
      END function

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


C
C ------------------------------------------------------------------------------
C
      real FUNCTION PLGNDR (L,M,X)
      implicit none
C
      INTEGER  L,M,I,LL
      REAL     X,PMM,SOMX2,FACT,PMMP1,PLL
C
      IF (M.LT.0.OR.M.GT.L.OR.ABS(X).GT.1.) then
        print*,  'bad arguments'
        stop
      endif
      PMM=1.0
      IF (M.GT.0) THEN
        SOMX2=SQRT((1.0-X)*(1.0+X))
        FACT=1.0
        DO I=1,M
            PMM=-PMM*FACT*SOMX2
            FACT=FACT+2.0
        ENDDO
      ENDIF
      IF (L.EQ.M) THEN
        PLGNDR=PMM
      ELSE
        PMMP1=X*(2*M+1)*PMM
        IF (L.EQ.M+1) THEN
            PLGNDR=PMMP1
        ELSE
            DO LL=M+2,L
                PLL=(X*(2*LL-1)*PMMP1-(LL+M-1)*PMM)/(LL-M)
                PMM=PMMP1
                PMMP1=PLL
            ENDDO
            PLGNDR=PLL
        ENDIF
      ENDIF
      RETURN
      END function




      end module
