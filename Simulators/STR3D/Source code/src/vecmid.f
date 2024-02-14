      SUBROUTINE VECMID(RR,RA,RB,INDX,NSIZE)
C                          ----------------------------
C                          DDOT WITH INDX TABLE   
C                                   IP=INDX(I)
C                                 RR=RA(I)*RB(IP)
C                          ----------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RA(NSIZE),RB(*)
      DIMENSION INDX(NSIZE)
C
C===  DO 1000 I=1,NSIZE
C===  IP=INDX(I)
C===  RR=RR+RA(I)*RB(IP)
C1000 CONTINUE
C                       + BLUS-LIKE +
C     A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C        CLEAN-UP LOOP
C
   20 M = MOD(NSIZE,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
       RR = RR + RA(I)*RB(INDX(I))
   30 CONTINUE
      IF( NSIZE .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,NSIZE,4
       RR = RR + RA(I)*RB(INDX(I))
       RR = RR + RA(I+1)*RB(INDX(I + 1))
       RR = RR + RA(I+2)*RB(INDX(I + 2))
       RR = RR + RA(I+3)*RB(INDX(I + 3))
   50 CONTINUE
C
      RETURN
      END
