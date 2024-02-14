      SUBROUTINE RMULT4(A,B,R,C,N)
C
C       SUB FOR MULTIPLY VECTOR      A=B+R*C
C
C       INPUT
C       R       SCOLAR TO BE MULTIPLIED
C       N       ORDER OF A
C       B       VECTOR TO BE MULTIPLIED
C       C       VECTOR TO BE MULTIPLIED
C
C       OUTPUT
C       A       OUTPUT VECTOR
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(*),B(*),C(*)
C                               ** MATRIX POINTER **
C===  DO 8000 I=1,N
C===  A(I)=B(I)+R*C(I)
C8000 CONTINUE
C                       + BLUS-LIKE +
C     A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        A(I) = B(I) + R*C(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        A(I) = B(I) + R*C(I)
        A(I + 1) = B(I + 1) + R*C(I + 1)
        A(I + 2) = B(I + 2) + R*C(I + 2)
        A(I + 3) = B(I + 3) + R*C(I + 3)
   50 CONTINUE
C
      RETURN
      END
