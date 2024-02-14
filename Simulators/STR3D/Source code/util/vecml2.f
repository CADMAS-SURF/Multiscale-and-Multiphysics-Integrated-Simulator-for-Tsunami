      SUBROUTINE VECML2(A,V,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(N)
C----&------------------------------------------------------------------
      A=0.
      DO I=1,N
        A=A+V(I)*V(I)
      ENDDO
      A=DSQRT(A)
C
      RETURN
      END
