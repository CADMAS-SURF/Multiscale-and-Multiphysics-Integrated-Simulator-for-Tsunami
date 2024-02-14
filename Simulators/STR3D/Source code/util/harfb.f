C***********************************************************************
      SUBROUTINE  HARFB( A,B,C, N1,N2,N3 )
C***********************************************************************
C  A = B X C  ( B MATRIX IS HARF MATRIX )
C
C  A      = A MATRIX   ( N1 X N3 )                        --- ( O )
C  B      = B MATRIX   ( ONE DIMENSION )                  --- ( I )
C  C      = C MATRIX   ( N2 X N3 )                        --- ( I )
C  N1     = NUMBER OF COLUMN                              --- ( I )
C  N2     = NUMBER OF COLUMN,ROWS                         --- ( I )
C  N1     = NUMBER OF ROWS                                --- ( I )
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(N1,N3),B(1),C(N2,N3)
C
C
C--- CALCULATE MATRIX ---
      IR   = N2
      LAST = 0
      DO 3000 I=1,N1
      DO 2000 J=1,N3
         IC     = 0
         A(I,J) = 0.0D0
C
         DO 1000 K=1,N2
            IF( I .LE. K )  THEN
                IAD = LAST + K - I + 1
             ELSE
                IAD = I  + IC
                IC  = IC + N2 - K
            ENDIF
C
            A(I,J) = A(I,J) + B(IAD)*C(K,J)
C
 1000    CONTINUE
 2000 CONTINUE
         LAST = LAST + IR
         IR   = IR - 1
 3000 CONTINUE
C
C
      RETURN
      END
