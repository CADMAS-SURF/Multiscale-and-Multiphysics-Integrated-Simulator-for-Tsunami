C***********************************************************************
      SUBROUTINE  HARFAB(A,B,C,N)
C***********************************************************************
C  A = B X C  ( A & B MATRIX IS HARF MATRIX )
C
C  A      = A MATRIX   ( ONE DIMENSION )                  --- ( O )
C  B      = B MATRIX   ( ONE DIMENSION )                  --- ( I )
C  C      = C MATRIX   ( N X N )                          --- ( I )
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(*),B(*),C(N,N)
C
      IC = 0
C
      DO I = 1, N
        DO J = I, N
          IC = IC + 1
          A(IC) = 0.
          DO K = 1, N
            IF( I <= K ) THEN
              IP = ( 2*N + 2 - I )*( I - 1 ) / 2 + K + 1 - I
            ELSE
              IP = ( 2*N + 2 - K )*( K - 1 ) / 2 + I + 1 - K
            ENDIF
            A(IC) = A(IC) + B(IP)*C(K,J)
          ENDDO
        ENDDO
      ENDDO
C
      END
