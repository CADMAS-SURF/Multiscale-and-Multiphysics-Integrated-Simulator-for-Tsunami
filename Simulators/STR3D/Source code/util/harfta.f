C***********************************************************************
      SUBROUTINE  HARFTA( A,B,C, N1,N2 )
C***********************************************************************
C  A = BT X C  ( A MATRIX IS HARF MATRIX )
C
C  A      = A MATRIX   ( ONE DIMENSION )                  --- ( O )
C  B      = B MATRIX   ( N1 X N2 )                        --- ( I )
C  C      = C MATRIX   ( N1 X N2 )                        --- ( I )
C  N1     = NUMBER OF COLUMN                              --- ( I )
C  N2     = NUMBER OF COLUMN,ROWS                         --- ( I )
C  N1     = NUMBER OF ROWS                                --- ( I )
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(1),B(N1,N2),C(N1,N2)
C
C
C--- CALCULATE MATRIX ---
      IC = 0
      DO 3000 I=1,N2
      DO 2000 J=I,N2
         IC    = IC + 1
         A(IC) = 0.0D0
C
         DO 1000 K=1,N1
            A(IC) = A(IC) + B(K,I)*C(K,J)
 1000    CONTINUE
 2000 CONTINUE
 3000 CONTINUE
C
C
      RETURN
      END
