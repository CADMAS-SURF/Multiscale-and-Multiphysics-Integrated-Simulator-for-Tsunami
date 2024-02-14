      SUBROUTINE TLOADSET(FTO,FCO,PFC,FCO2,NNOD,TIM3,BETA,IDLOAD,KK,ICRD
     &                   ,TR,GRID,IELM,AMAT,RODA,BARD,IDLD,SDLD,SIDL
     &                   ,LIDL,ITL1,ITD1,TBD1,LOAD,SLOD,SILD,LILD,IFC
     &                   ,NFC,FC,IPL4,NPL4,PLD4,IGRV,GRAV,IRFC,RFRC,IPFC
     &                   ,AFC,IPND,PPND,POS,IDYN,ICPL,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION LOAD(2,*),SLOD(*),SILD(*),LILD(*),IFC(2,*),NFC(2,*)
     &         ,FC(6,*),IPL4(2,*),NPL4(4,*),PLD4(4,*),KK(*),GRID(3,NNOD)
     &         ,IELM(*),AMAT(33,*),RODA(*),BARD(6,*),IGRV(*),GRAV(3,*)
     &         ,IRFC(2,*),RFRC(3,*),TR(12,*),ICRD(*),IDLD(2,*),SDLD(*)
     &         ,SIDL(*),LIDL(*),ITL1(4,*),ITD1(*),TBD1(2,*),FTO(6,NNOD)
     &         ,FCO(6,NNOD,3),PFC(6,NNOD),IPFC(10,*),AFC(*),IPND(NNOD)
     &         ,PPND(NNOD),POS(3,NNOD),FCO2(6,NNOD)
C-----------------------------------------------------------------------
      NDLD  = KK(66)
      NTLD1 = KK(77)
C
      CALL CLEAR1(FCO(1,1,3),6*NNOD)
      CALL CLEAR1(FCO2,6*NNOD)
C
      IDLFLG=0
C
      DO 100 I=1,NDLD
        IF( IDLD(1,I) .EQ. IDLOAD ) THEN
          CALL ADDSET(I,IDLD,IS,NN)
          S=SDLD(I)
          IDLFLG=1
          GOTO 200
        ENDIF
  100 CONTINUE
C
  200 IF(IDLFLG .EQ. 1) THEN
C
        DO 300 I=1,NN
C
          LI=LIDL(IS-1+I)
          SI=SIDL(IS-1+I)
C
          DO 310 J=1,NTLD1
C
            ID=ITL1(1,J)
            ITYPE=ITL1(3,J)
C
            IF( ID .EQ. LI .AND. ITYPE .EQ. 0 ) THEN
C
              IEX=ITL1(2,J)
C
              CALL STLDSET(PFC,IEX,KK,ICRD,TR,GRID,IELM,AMAT,RODA,BARD
     &                    ,LOAD,SLOD,SILD,LILD,IFC,NFC,FC,IPL4,NPL4,PLD4
     &                    ,IGRV,GRAV,IRFC,RFRC,ITO)
C
              ITF=ITL1(4,J)
              CALL ADDSET2(ITF,ITD1,IP,N)
              CALL INTPL(FAC,TIM3,TBD1(1,IP),N)
C
              FAC=FAC*S*SI
C
              CALL RMULT2(FCO(1,1,3),PFC,FAC,6*NNOD)
C
              GOTO 300
C
            ENDIF
C
  310     CONTINUE
C
  300   CONTINUE
C
      ELSE
C
        DO 410 J=1,NTLD1
C
          ID=ITL1(1,J)
          ITYPE=ITL1(3,J)
C
          IF( ID .EQ. IDLOAD .AND. ITYPE .EQ. 0 ) THEN
C
            IEX=ITL1(2,J)
C
            CALL STLDSET(PFC,IEX,KK,ICRD,TR,GRID,IELM,AMAT,RODA,BARD
     &                  ,LOAD,SLOD,SILD,LILD,IFC,NFC,FC,IPL4,NPL4,PLD4
     &                  ,IGRV,GRAV,IRFC,RFRC,ITO)
C
            ITF=ITL1(4,J)
            CALL ADDSET2(ITF,ITD1,IP,N)
            CALL INTPL(FAC,TIM3,TBD1(1,IP),N)
C
            CALL RMULT1(FCO(1,1,3),PFC,FAC,6*NNOD)
C
            GOTO 500
C
          ENDIF
C
  410   CONTINUE
C
      ENDIF
C
  500 CONTINUE
C
      IF( ICPL > 0 )
     &  CALL PLD_CADMAS(FCO(1,1,3),FCO2,POS,IELM,KK(37),KK(81),IPFC,AFC
     &                 ,IPND,PPND,ITO)
C
      IF( IDYN == 1 ) THEN
        FTO(:,:) = BETA * FCO(:,:,1) + ( 1.D0 - 2.D0*BETA ) * FCO(:,:,2)
     &           + BETA * FCO(:,:,3) + FCO2(:,:)
      ELSEIF( IDYN == 0 ) THEN
        FTO(:,:) = FCO(:,:,3) + FCO2(:,:)
      ENDIF
C
      END
