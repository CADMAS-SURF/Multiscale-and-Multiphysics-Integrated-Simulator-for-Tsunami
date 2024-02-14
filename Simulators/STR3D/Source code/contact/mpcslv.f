      SUBROUTINE MPCSLV(NROW,B,C,IDL,IROW_L,ICOL_L,IROW_R,ICOL_R,CIDEP
     &                 ,IDX,IFRH,CRH,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION B(*),C(*),IROW_L(NROW+1),ICOL_L(*),IROW_R(NROW+1)
     &         ,ICOL_R(*),CIDEP(NROW),IFRH(*),CRH(*),IDX(2,NROW)
     &         ,IDL(NROW)
C-----------------------------------------------------------------------
C
C     --- [B],[C] ---
C
      DO 200 I=1,NROW
        ID=IDL(I)
        B(ID)=CIDEP(I)
        IS=IDX(1,I)
        IE=IDX(2,I)
        DO 210 J=IS,IE
          IC=IFRH(J)
          IF( IC .LT. 0 ) THEN
            IS1=IROW_L(I)
            IE1=IROW_L(I+1)-1
            DO 211 K=IS1,IE1
              IF( ICOL_L(K) .EQ. -IC ) THEN
                B(K)=B(K)-CRH(J)
                GOTO 210
              ENDIF
  211       CONTINUE
          ELSEIF( IC .GT. 0 ) THEN
            IS1=IROW_R(I)
            IE1=IROW_R(I+1)-1
            DO 212 K=IS1,IE1
              IF( ICOL_R(K) .EQ. IC ) THEN
                C(K)=C(K)+CRH(J)
                GOTO 210
              ENDIF
  212       CONTINUE
          ENDIF
          WRITE(ITO,*) 'NO ADDRESS FOR [B],[C], STOP IN SUB. MPCSLV!'
          CALL ERRSTP(90,ITO)
  210   CONTINUE
  200 CONTINUE
C
C     --- FORWARD ELIMINATION ---
C
      ID=IDL(1)
      IE=IROW_L(2)-1
      DO 500 J=ID+1,IE
        B(J)=B(J)/B(ID)
  500 CONTINUE
C
      ISR=IROW_R(1)
      IER=IROW_R(2)-1
      DO 501 J=ISR,IER
        IF( C(J) .NE. 0. ) C(J)=C(J)/B(ID)
  501 CONTINUE
C
      DO 300 I=2,NROW
C
        IS=IROW_L(I)
        IE=IROW_L(I+1)-1
        ID=IDL(I)
C
        ISR=IROW_R(I)
        IER=IROW_R(I+1)-1
C
        DO 310 II=IS,ID-1
C
C         --- Ith ROW ELIMINATION BY Jth ROW ---
C
          J=ICOL_L(II)
          R=B(II)
C
          JD=IDL(J)
          JE=IROW_L(J+1)-1
          IP=II+1
C
          DO 311 K=JD+1,JE
            IC=ICOL_L(K)
            DO 312 L=IP,IE
              IF( ICOL_L(L) .EQ. IC ) THEN
                B(L)=B(L)-B(K)*R
                EXIT
              ENDIF
  312       CONTINUE
            IP=L+1
  311     CONTINUE
C
          JSR=IROW_R(J)
          JER=IROW_R(J+1)-1
          IPR=ISR
          DO 313 K=JSR,JER
            IF( C(K) .EQ. 0. ) GOTO 313
            IC=ICOL_R(K)
            DO 314 L=IPR,IER
              IF( ICOL_R(L) .EQ. IC ) THEN
                C(L)=C(L)-C(K)*R
                EXIT
              ENDIF
  314       CONTINUE
            IPR=L+1
  313     CONTINUE
C
  310   CONTINUE
C
        DO 320 J=ID+1,IE
          B(J)=B(J)/B(ID)
  320   CONTINUE
C
        DO 330 J=ISR,IER
          IF( C(J) .NE. 0. ) C(J)=C(J)/B(ID)
  330   CONTINUE
C
  300 CONTINUE
C
C     --- BACKWARD ELIMINATION ---
C
      DO 400 I=NROW-1,1,-1
C
        ID=IDL(I)
        IE=IROW_L(I+1)-1
C
        ISR=IROW_R(I)
        IER=IROW_R(I+1)-1
C
        DO 410 II=IE,ID+1,-1
C
C         --- Ith ROW ELIMINATION BY Jth ROW ---
C
          J=ICOL_L(II)
          R=B(II)
C
          JSR=IROW_R(J)
          JER=IROW_R(J+1)-1
          IPR=ISR
          DO 411 K=JSR,JER
            IC=ICOL_R(K)
            DO 412 L=IPR,IER
              IF( ICOL_R(L) .EQ. IC ) THEN
                C(L)=C(L)-C(K)*R
                EXIT
              ENDIF
  412       CONTINUE
            IPR=L+1
  411     CONTINUE
C
  410   CONTINUE
C
  400 CONTINUE
C
      RETURN
      END
