      SUBROUTINE STLDSET(FTO,ISID,KK,ICRD,TR,GRID,IELM,AMAT,RODA,BARD
     &                  ,LOAD,SLOD,SILD,LILD,IFC,NFC,FC,IPL4,NPL4,PLD4
     &                  ,IGRV,GRAV,IRFC,RFRC,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION LOAD(2,*),SLOD(*),SILD(*),LILD(*),IFC(2,*),NFC(2,*)
     &         ,FC(6,*),IPL4(2,*),NPL4(4,*),PLD4(4,*),FTO(6,*),GRID(3,*)
     &         ,IELM(*),KK(*),IGRV(*),GRAV(3,*),IRFC(2,*),RFRC(3,*)
     &         ,AMAT(33,*),TR(12,*),ICRD(*),RODA(*),BARD(6,*)
C-----------------------------------------------------------------------
      NNOD  = KK(8)
      NELM  = KK(12)
      NM    = KK(37)
      NLOAD = KK(40)
      NIFC  = KK(42)
      NIPL4 = KK(44)
      NIGRV = KK(60)
      NIRFC = KK(61)
C
      CALL CLEAR1(FTO,NNOD*6)
C
      LOADFLG=0
C
      DO 100 I=1,NLOAD
        IF(LOAD(1,I) .EQ. ISID) THEN
          CALL ADDSET(I,LOAD,IS,NN)
          S=SLOD(I)
          LOADFLG=1
          GOTO 200
        ENDIF
  100 CONTINUE
C
  200 IF(LOADFLG .EQ. 1) THEN
C
        DO 300 I=1,NN
          SI=S*SILD(IS-1+I)
          DO 310 J=1,NIFC
            IF(IFC(1,J) .EQ. LILD(IS-1+I)) THEN
              CALL ADDSET(J,IFC,ISF,NNF)
              CALL FORCE(NNF,FC(1,ISF),NFC(1,ISF),SI,ICRD,TR,GRID,FTO
     &                  ,ITO)
              GOTO 320
            ENDIF
  310     CONTINUE
C
  320     DO 330 J=1,NIPL4
            IF(IPL4(1,J) .EQ. LILD(IS-1+I)) THEN
              CALL ADDSET(J,IPL4,ISP,NNP)
              CALL PLOAD4(NNP,NPL4(1,ISP),PLD4(1,ISP),IELM,NM,ICRD,TR
     &                   ,GRID,SI,FTO,ITO)
              GOTO 340
            ENDIF
  330     CONTINUE
C
  340     DO 350 J=1,NIGRV
            IF(IGRV(J) .EQ. LILD(IS-1+I)) THEN
              CALL BDYFC(FTO,SI,1,GRAV(1,J),IWK,WK,GRID,NELM,NM,IELM
     &                  ,AMAT,RODA,BARD,ITO)
              GOTO 360
            ENDIF
  350     CONTINUE
C
  360     DO 370 J=1,NIRFC
            IF(IRFC(1,J) .EQ. LILD(IS-1+I)) THEN
              CALL BDYFC(FTO,SI,0,WK,IRFC(2,J),RFRC(1,J),GRID,NELM,NM
     &                  ,IELM,AMAT,RODA,BARD,ITO)
              GOTO 300
            ENDIF
  370     CONTINUE
  300   CONTINUE
C
      ELSE
C
        SI=1.D0
        DO 410 J=1,NIFC
          IF(IFC(1,J) .EQ. ISID) THEN
            CALL ADDSET(J,IFC,ISF,NNF)
            CALL FORCE(NNF,FC(1,ISF),NFC(1,ISF),SI,ICRD,TR,GRID,FTO,ITO)
            GOTO 420
          ENDIF
  410   CONTINUE
C
  420   DO 430 J=1,NIPL4
          IF(IPL4(1,J) .EQ. ISID) THEN
            CALL ADDSET(J,IPL4,ISP,NNP)
            CALL PLOAD4(NNP,NPL4(1,ISP),PLD4(1,ISP),IELM,NM,ICRD,TR
     &                 ,GRID,SI,FTO,ITO)
            GOTO 440
          ENDIF
  430   CONTINUE
C
  440   DO 450 J=1,NIGRV
          IF(IGRV(J) .EQ. ISID) THEN
            CALL BDYFC(FTO,SI,1,GRAV(1,J),IWK,WK,GRID,NELM,NM,IELM,AMAT
     &                ,RODA,BARD,ITO)
            GOTO 460
          ENDIF
  450   CONTINUE
C
  460   DO 470 J=1,NIRFC
          IF(IRFC(1,J) .EQ. ISID) THEN
            CALL BDYFC(FTO,SI,0,WK,IRFC(2,J),RFRC(1,J),GRID,NELM,NM,IELM
     &                ,AMAT,RODA,BARD,ITO)
            GOTO 500
          ENDIF
  470   CONTINUE
C
      ENDIF
C
  500 RETURN
      END
