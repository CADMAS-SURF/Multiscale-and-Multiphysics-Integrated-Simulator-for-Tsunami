C***********************************************************************
      SUBROUTINE  SF_AXBT( C,A,B,N1,N2,N3 )
C***********************************************************************
C  CALCULATE  C = A X BT
C
C  C      = C MATRIX   ( N1 X N3 )                        --- ( O )
C  A      = A MATRIX   ( N1 X N2 )                        --- ( I )
C  B      = B MATRIX   ( N3 X N2 )                        --- ( I )
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  C(N1,N3),A(N1,N2),B(N3,N2)
C
      DO 3000 I=1,N1
      DO 2000 J=1,N3
        C(I,J) = 0.0D0
        DO 1000 K=1,N2
          C(I,J) = C(I,J) + A(I,K)*B(J,K)
 1000   CONTINUE
 2000 CONTINUE
 3000 CONTINUE
C
      RETURN
      END
