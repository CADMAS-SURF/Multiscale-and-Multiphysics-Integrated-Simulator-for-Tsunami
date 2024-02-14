      SUBROUTINE SF_DIRCOS(A,B,N)
C
C     {A}={B}/|B|
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N)
C
      XL=0.
      DO 100 I=1,N
        XL=XL+B(I)*B(I)
  100 CONTINUE
      XL=DSQRT(XL)
      DO 200 I=1,N
        A(I)=B(I)/XL
  200 CONTINUE
C
      RETURN
      END
