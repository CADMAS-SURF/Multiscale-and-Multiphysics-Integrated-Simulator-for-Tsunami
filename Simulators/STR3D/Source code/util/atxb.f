C***********************************************************************
      SUBROUTINE  ATXB( C,A,B,N1,N2,N3 )
C***********************************************************************
C  CALCULATE  C = AT X B
C
C  C      = C MATRIX   ( N2 X N3 )                        --- ( O )
C  A      = A MATRIX   ( N1 X N2 )                        --- ( I )
C  B      = B MATRIX   ( N1 X N3 )                        --- ( I )
C  N1     = NUMBER OF COLUMN                              --- ( I )
C  N2     = NUMBER OF COLUMN,ROWS                         --- ( I )
C  N3     = NUMBER OF ROWS                                --- ( I )
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  C(N2,N3),A(N1,N2),B(N1,N3)
C
C
C--- CALCULATE MATRIX ---
      DO 3000 I=1,N3
      DO 2000 J=1,N2
         C(J,I) = 0.0D0
C
         DO 1000 K=1,N1
            C(J,I) = C(J,I) + A(K,J)*B(K,I)
 1000    CONTINUE
 2000 CONTINUE
 3000 CONTINUE
C
C
      RETURN
      END
