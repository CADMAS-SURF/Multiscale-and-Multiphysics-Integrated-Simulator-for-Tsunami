      SUBROUTINE SF_STRIA(S,COOD,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION COOD(N,3),V12(3),V13(3)
C-----------------------------------------------------------------------
      V12(1:N) = COOD(1:N,2) - COOD(1:N,1)
      V13(1:N) = COOD(1:N,3) - COOD(1:N,1)
      IF( N .EQ. 3 ) THEN
        CALL SF_CROSS1(V12,V13,S)
      ELSEIF( N .EQ. 2 ) THEN
        S = DABS( V12(1)*V13(2) - V12(2)*V13(1) )
      ENDIF
      S = S*.5D0
C
      RETURN
      END
