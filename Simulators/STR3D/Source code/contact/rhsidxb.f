      SUBROUTINE RHSIDXB(NROW,IROW_L,ICOL_L,IROW_R2,ICOL_R2,IROW_R3
     &                  ,ICOL_R3,NSIZ_R3)
C
      DIMENSION IROW_R2(NROW+1),ICOL_R2(*),IROW_R3(NROW+1),ICOL_R3(*)
     &         ,IROW_L(NROW+1),ICOL_L(*)
C-----------------------------------------------------------------------
      IROW_R3(1)=1
      IS=IROW_R2(NROW)
      IE=IROW_R2(NROW+1)-1
      N=IE-IS+1
      CALL SHIFT0(ICOL_R3,ICOL_R2(IS),N)
      IROW_R3(2)=IROW_R3(1)+N
C
      DO 100 II=2,NROW
C
        I=NROW+1-II
C
        IS=IROW_R2(I)
        IE=IROW_R2(I+1)-1
        N=IE-IS+1
        IP0=IROW_R3(II)
        CALL SHIFT0(ICOL_R3(IP0),ICOL_R2(IS),N)
C
        IS=IROW_L(I)
        IE=IROW_L(I+1)-1
C
        DO 110 IP=IE,IS,-1
C
          J=ICOL_L(IP)
          IF( J .EQ. I ) EXIT
          JJ=NROW+1-J
          JS=IROW_R3(JJ)
          JE=IROW_R3(JJ+1)-1
          KS=1
C
          DO 111 K=JS,JE
            IC=ICOL_R3(K)
            CALL ORDER2(N,KS,ICOL_R3(IP0),IC)
  111     CONTINUE
C
  110   CONTINUE
C
        IROW_R3(II+1)=IROW_R3(II)+N
C
  100 CONTINUE
C
      NSIZ_R3=IROW_R3(NROW+1)-1
C
      RETURN
      END
