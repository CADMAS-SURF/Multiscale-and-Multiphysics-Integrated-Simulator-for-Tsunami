C***********************************************************************
      SUBROUTINE  SHIFT1( A,B,N )
C***********************************************************************
C  A = B  ---  DATA SHIFT
C
C  A      = SHIFT AREA                                    --- ( O )
C  B      = SHIFT DATA                                    --- ( I )
C  N      = NUMBER OF SHIFT DATA                          --- ( I )
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(N),B(N)
C
C
C--- SHIFT ---
      DO 1000 I=1,N
         A(I) = B(I)
 1000 CONTINUE
C
C
      RETURN
      END
