      SUBROUTINE LHSIDX(NROW,IROW_L,ICOL_L,IROW_L2,ICOL_L2,NSIZ_L2,IDL)
C
      DIMENSION IROW_L(NROW+1),ICOL_L(*),IROW_L2(NROW+1),ICOL_L2(*)
     &         ,IDL(NROW)
C-----------------------------------------------------------------------
      IROW_L2(1)=1
      N=IROW_L(2)-1
      CALL SHIFT0(ICOL_L2,ICOL_L,N)
      IROW_L2(2)=IROW_L2(1)+N
C
      DO 100 I=2,NROW
C
        IS=IROW_L(I)
        IE=IROW_L(I+1)-1
        N=IE-IS+1
        IP0=IROW_L2(I)
        CALL SHIFT0(ICOL_L2(IP0),ICOL_L(IS),N)
        IP=IP0-1
C
  110   CONTINUE
C
          IP=IP+1
          J=ICOL_L2(IP)
          IF( J .EQ. I ) GOTO 120
          IS=IROW_L2(J)
          IE=IROW_L2(J+1)-1
          KS=1
C
          DO 111 K=IS,IE
            IC=ICOL_L2(K)
            IF( IC .GT. J ) CALL ORDER2(N,KS,ICOL_L2(IP0),IC)
  111     CONTINUE
C
        GOTO 110
C
  120   IROW_L2(I+1)=IROW_L2(I)+N
C
  100 CONTINUE
C
      NSIZ_L2=IROW_L2(NROW+1)-1
C
      DO 200 I=1,NROW
        IS=IROW_L2(I)
        IE=IROW_L2(I+1)-1
        DO 210 J=IS,IE
          IF( ICOL_L2(J) .EQ. I ) THEN
            IDL(I)=J
            EXIT
          ENDIF
  210   CONTINUE
  200 CONTINUE
C
      RETURN
      END
