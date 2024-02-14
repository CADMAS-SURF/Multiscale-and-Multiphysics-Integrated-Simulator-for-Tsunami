      SUBROUTINE RMULT3(A,B,R,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(*),B(*)
C                         
      DO 8000 I=1,N
      A(I)=A(I)+R*B(I)*B(I)
 8000 CONTINUE
C
      RETURN
      END
