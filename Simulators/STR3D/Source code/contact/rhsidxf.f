      SUBROUTINE RHSIDXF(NROW,IROW_L,ICOL_L,IROW_R,ICOL_R,IROW_R2
     &                  ,ICOL_R2,NSIZ_R2)
C
      DIMENSION IROW_R(NROW+1),ICOL_R(*),IROW_R2(NROW+1),ICOL_R2(*)
     &         ,IROW_L(NROW+1),ICOL_L(*)
C-----------------------------------------------------------------------
      IROW_R2(1)=1
      N=IROW_R(2)-1
      CALL SHIFT0(ICOL_R2,ICOL_R,N)
      IROW_R2(2)=IROW_R2(1)+N
C
      DO 100 I=2,NROW
C
        IS=IROW_R(I)
        IE=IROW_R(I+1)-1
        N=IE-IS+1
        IP0=IROW_R2(I)
        CALL SHIFT0(ICOL_R2(IP0),ICOL_R(IS),N)
C
        IS=IROW_L(I)
        IE=IROW_L(I+1)-1
C
        DO 110 IP=IS,IE
C
          J=ICOL_L(IP)
          IF( J .EQ. I ) EXIT
          JS=IROW_R2(J)
          JE=IROW_R2(J+1)-1
          KS=1
C
          DO 111 K=JS,JE
            IC=ICOL_R2(K)
            CALL ORDER2(N,KS,ICOL_R2(IP0),IC)
  111     CONTINUE
C
  110   CONTINUE
C
        IROW_R2(I+1)=IROW_R2(I)+N
C
  100 CONTINUE
C
      NSIZ_R2=IROW_R2(NROW+1)-1
C
      RETURN
      END
