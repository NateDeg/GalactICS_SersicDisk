
      module FactorialMod
      use GammaFnRoutines
      implicit none
      contains

C
C ------------------------------------------------------------------------------
C
      real FUNCTION FACTRL (N)
C
      INTEGER  N,NTOP,J
      REAL     A(33)
C
      DATA  NTOP,A(1)/0,1.0/
C
      IF (N.LT.0) THEN
        print*, 'negative factorial'
        stop
      ELSE IF (N.LE.NTOP) THEN
        FACTRL=A(N+1)
      ELSE IF (N.LE.32) THEN
        DO J=NTOP+1,N
            A(J+1)=J*A(J)
        ENDDO
        NTOP=N
        FACTRL=A(N+1)
      ELSE
        FACTRL=EXP(GAMMLN(float(N)+1.0))
      ENDIF
      RETURN
      END function
C
C ------------------------------------------------------------------------------

      end module
