      SUBROUTINE RHSIDXR(NROW,IROW_R,ICOL_R,IROW_R3,ICOL_R3)
C
      DIMENSION IROW_R(NROW+1),ICOL_R(*),IROW_R3(NROW+1),ICOL_R3(*)
C-----------------------------------------------------------------------
      IROW_R(1)=1
C
      DO 100 I=1,NROW
C
        II=NROW+1-I
C
        IS=IROW_R3(II)
        IE=IROW_R3(II+1)-1
        N=IE-IS+1
        IP=IROW_R(I)
        CALL SHIFT0(ICOL_R(IP),ICOL_R3(IS),N)
C
        IROW_R(I+1)=IROW_R(I)+N
C
  100 CONTINUE
C
      RETURN
      END
