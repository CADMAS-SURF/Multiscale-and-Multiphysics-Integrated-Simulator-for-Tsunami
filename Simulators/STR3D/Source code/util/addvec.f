      SUBROUTINE ADDVEC(RVEC,RA,RB,NSIZE)
C                          ----------------------------
C                          ADDITION VECTOR       (REAL)
C                                 RVEC(I)=RA(I)+RB(I)
C                          ----------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RVEC(NSIZE),RA(NSIZE),RB(NSIZE)
C
      DO 1000 I=1,NSIZE
      RVEC(I)=RA(I)+RB(I)
 1000 CONTINUE
C
      RETURN
      END
