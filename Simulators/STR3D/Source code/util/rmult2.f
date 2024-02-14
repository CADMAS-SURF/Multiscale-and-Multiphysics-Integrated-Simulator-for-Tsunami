      SUBROUTINE RMULT2(A,B,R,N)
C
C       SUB FOR MULTIPLY VECTOR      A=A+R*B
C
C       INPUT
C       R       SCOLAR TO BE MULTIPLIED
C       N       ORDER OF A
C       B       VECTOR TO BE MULTIPLIED
C
C       OUTPUT
C       A       OUTPUT VECTOR
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(*),B(*)
C                               ** MATRIX POINTER **
      DO 8000 I=1,N
      A(I)=A(I)+R*B(I)
 8000 CONTINUE
C
      RETURN
      END
