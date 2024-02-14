C***********************************************************************
      SUBROUTINE  CLEAR1( T,N )
C***********************************************************************
C  0 CLEAR OF REAL TABLE
C
C  T      = CLEAR TABLE                                   --- ( O )
C  N      = NUMBER OF IT TABLE                            --- ( I )
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  T(N)
C
C
      IF( N .LE. 0 )  RETURN
C
C--- 0 CLEAR ---
      DO 1000 I=1,N
         T(I) = 0.0D0
 1000 CONTINUE
C
C
      RETURN
      END
