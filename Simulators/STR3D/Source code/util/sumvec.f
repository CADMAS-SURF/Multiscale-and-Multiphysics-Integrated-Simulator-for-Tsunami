      SUBROUTINE SUMVEC(R,A,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N)
C
      R=0.
      DO 1000 I=1,N
        R=R+A(I)
 1000 CONTINUE
C
      RETURN
      END
