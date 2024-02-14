      SUBROUTINE  HARFC( A,B,C, N1,N2 )
C
C     A = B X C  ( C MATRIX IS HARF MATRIX )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(N1,N2),B(N1,N2),C(*)
C----&------------------------------------------------------------------
      DO 100 I=1,N1
        DO 200 J=1,N2
          A(I,J)=0.
          DO 300 K=1,N2
C
            IF(K .LE. J) THEN
              IROW=K
              ICOL=J
            ELSE
              IROW=J
              ICOL=K
            ENDIF
C
            IP=(2*N2-IROW+2)*(IROW-1)/2+ICOL-IROW+1
C
            A(I,J)=A(I,J)+B(I,K)*C(IP)          
C
  300     CONTINUE
  200   CONTINUE
  100 CONTINUE
C
      RETURN
      END
