      SUBROUTINE VECMID3(RR,RA,RB,INDX,NSIZE)
C                          ----------------------------
C                          DDOT WITH INDX TABLE   
C                                   IP=INDX(I)
C                                 RR=RA(I)*RB(IP)
C                          ----------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RA(NSIZE),RB(*)
      DIMENSION INDX(NSIZE)
C
C===  RR=0.0D0
C===  DO 1000 I=1,NSIZE
C===  IP=INDX(I)
C===  RR=RR+RA(I)*RB(IP)
C1000 CONTINUE
C                       + BLUS-LIKE +
c     a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
c        clean-up loop
c
      RR=0.0D0
   20 m = mod(NSIZE,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
       RR = RR + RA(I)*RB(INDX(I))
   30 continue
      if( NSIZE .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,NSIZE,4
       RR = RR + RA(I)*RB(INDX(I))
       RR = RR + RA(I+1)*RB(INDX(I + 1))
       RR = RR + RA(I+2)*RB(INDX(I + 2))
       RR = RR + RA(I+3)*RB(INDX(I + 3))
   50 continue
C
      RETURN
      END
