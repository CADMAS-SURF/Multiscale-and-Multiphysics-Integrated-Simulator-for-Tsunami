      SUBROUTINE  HARFTA2( A,B,C, N1,N2 )
C
C     A = B X CT  ( A MATRIX IS HARF MATRIX )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(1),B(N1,N2),C(N1,N2)
C----&------------------------------------------------------------------
      IC = 0
      DO 3000 I=1,N1
      DO 2000 J=I,N1
         IC    = IC + 1
         A(IC) = 0.0D0
C
         DO 1000 K=1,N2
            A(IC) = A(IC) + B(I,K)*C(J,K)
 1000    CONTINUE
 2000 CONTINUE
 3000 CONTINUE
C
C
      RETURN
      END
