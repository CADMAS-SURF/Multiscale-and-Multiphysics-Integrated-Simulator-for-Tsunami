      SUBROUTINE RESFRC(RES,RFCO,RFCI,ICONV,KK,RR,INDG,INDOF,INDMPC
     &                 ,MPCF,RMPC,FTO,FTI,INDOP,FLO,FLI,FNRM,TIM,ISTEP
     &                 ,ITER,ITER2,ITO)
C
      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),RR(*),RES(*),RNRM(3),RESMAX(2),NODMAX(2),JMAX(2)
     &         ,INDOF(6,*),FTO(6,*),FTI(6,*),INDMPC(2,6,*),MPCF(2,*)
     &         ,RMPC(*),INDG(*),FNRM(3),RFCI(6,*),RFCO(6,*),DUM(3)
     &         ,FNRM2(3),INDOP(*),FLO(*),FLI(*)
C-----------------------------------------------------------------------
      NNOD=KK(8)
      NNODI=KK(26)
      NNODC=KK(28)
      NNODX=KK(31)
      NIGSF=KK(94)
      NIGSFC=KK(108)
C
      CALL CLEAR1(RFCI,NNODI*6)
      CALL CLEAR1(RFCO,NNODI*6)
      CALL CLEAR1(RNRM,3)
      CALL CLEAR1(RESMAX,2)
      CALL CLEAR0(NODMAX,2)
      CALL CLEAR0(JMAX,2)
C
C     --- CONSTRAINT FORCE & RESIDUAL FORCE ---
C
      DO 400 I=1,NNOD+NNODC+NIGSF+NIGSFC+NNODX
        IF(I .GT. NNOD .AND. I .LE. NNOD+NNODC+NIGSF+NIGSFC) GOTO 400
        DO 410 J=1,6
          IF(INDOF(J,I) .EQ. -2) THEN
            RFCI(J,I)=FTI(J,I)-FTO(J,I)
            IS=INDMPC(1,J,I)
            IE=INDMPC(2,J,I)
            DO 420 IP=IS,IE
              NODE=MPCF(1,IP)
              K   =MPCF(2,IP)
              IF(NODE .LE. NNODI)
     &          RFCI(K,NODE)=RFCI(K,NODE)-RMPC(IP)*RFCI(J,I)
  420       CONTINUE
          ENDIF
  410   CONTINUE
  400 CONTINUE
C
      DO 500 I=1,NNODI
        DO 510 J=1,6
          IFR=INDOF(J,I)
          IF( IFR .EQ. 0 .OR. IFR .EQ. -1 ) THEN
            RFCO(J,I) = FTI(J,I) - FTO(J,I) - RFCI(J,I)
          ELSEIF( IFR .GT. 0 ) THEN
            RES(IFR) = FTO(J,I) - FTI(J,I) + RFCI(J,I)
          ENDIF
  510   CONTINUE
  500 CONTINUE
C
      IF( KK(21) > 10 ) THEN
        DO I = 1, NNODI
          IFR = INDOP(I)
          IF( IFR > 0 ) RES(IFR) = FLO(I) - FLI(I)
        ENDDO
      ENDIF
C
      DO 200 I=1,NNODI
C
        DO 210 J=1,3
          IFR=INDOF(J,I)
          IF(IFR .GT. 0) THEN
            RNRM(1)=RNRM(1)+RES(IFR)*RES(IFR)
            IF( DABS( RES(IFR) ) .GT. DABS( RESMAX(1) ) ) THEN
              RESMAX(1)=RES(IFR)
              NODMAX(1)=INDG(I)
              JMAX(1)=J
            ENDIF
          ENDIF
  210   CONTINUE
C
        DO 220 J=4,6
          IFR=INDOF(J,I)
          IF(IFR .GT. 0) THEN
            RNRM(2)=RNRM(2)+RES(IFR)*RES(IFR)
            IF( DABS( RES(IFR) ) .GT. DABS( RESMAX(2) ) ) THEN
              RESMAX(2)=RES(IFR)
              NODMAX(2)=INDG(I)
              JMAX(2)=J
            ENDIF
          ENDIF
  220   CONTINUE
C
  200 CONTINUE
C
      IF( KK(21) > 10 ) THEN
        DO I = 1, NNODI
          IFR = INDOP(I)
          IF( IFR > 0 ) RNRM(3) = RNRM(3) + RES(IFR) * RES(IFR)
        ENDDO
      ENDIF
C
      IF( MYRANK == 0 ) THEN
C
        IF(RNRM(1) .GT. 0.) RNRM(1)=DSQRT( RNRM(1) )
        IF(RNRM(2) .GT. 0.) RNRM(2)=DSQRT( RNRM(2) )
        IF(RNRM(3) .GT. 0.) RNRM(3)=DSQRT( RNRM(3) )
C
        ICONV = 0
C
        IF( ITER > 0 ) THEN
          RRES = RNRM(1) / FNRM(1)
          IF( RRES < RR(6) .OR. RNRM(1) < RR(3) ) ICONV = 1
        ENDIF
C
        IF( ITER == 0 ) THEN
C
          IF( ITER2 == 1 ) WRITE(ITO,'(/A,I0,A,1PE10.4,A)')
     &      ' ***** STEP NO. = ',ISTEP,'   TIME = ',TIM,' *****'
C
          WRITE(ITO,'(/A//A,A)') 
     &      '   * RESIDUAL FORCE',
     &      '     ITER     RNRM1       FNRM1       RRES        RNRM2  ',
     &      '     FNRM2       RNRM3       FNRM3'
C
        ELSE
C
          WRITE(ITO,'(7X,I2,1P7E12.4)') ITER,RNRM(1),FNRM(1),RRES
     &                                 ,RNRM(2),FNRM(2),RNRM(3),FNRM(3)
C
        ENDIF
C
      ELSE
C
        IF( MYRANK == 1 ) THEN
C
          CALL M_MPI_SEND_I(3,1,0)  ! SEND IOP=3 TO GLB_COMM
C
          CALL M_MPI_SEND_I(ISTEP,1,0)
          CALL M_MPI_SEND_D(TIM,1,0)
          CALL M_MPI_SEND_I(ITER2,1,0)
          CALL M_MPI_SEND_I(ITER,1,0)
C
        ENDIF
C
        FNRM2(:) = FNRM(:) * FNRM(:)
        CALL M_MPI_REDUCE_D(FNRM2,DUM,3,0)
C
        CALL M_MPI_REDUCE_D(RNRM,DUM,3,0)
C
        CALL M_MPI_BCAST_I(ICONV,1)
C
      ENDIF
C
      END
