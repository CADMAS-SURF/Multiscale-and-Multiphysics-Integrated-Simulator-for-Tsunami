      SUBROUTINE SPCSET(ISID,INDOF,KK,IELM,NM,ISPA,NSPA,ISP1,NSP1
     &                 ,ISPC,NSPC,SPC,UG,IMODE,IDYN,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (IGNR=20)
      DIMENSION KK(*),ISPA(2,*),ISP1(2,*),NSPA(*),INDOF(6,*),NSP1(7,*)
     &         ,ISPC(2,*),NSPC(7,*),SPC(6,*),UG(6,*),IELM(NM,*)
      INTEGER, POINTER :: IWK(:)
C-----------------------------------------------------------------------
      NNOD  = KK( 8)
      NELM  = KK(12)
      NISPA = KK(46)
      NISP1 = KK(48)
      NISPC = KK(56)
C
      ALLOCATE( IWK(NNOD) )
C
      IF(ISID .EQ. 0) GOTO 500
C
      IADDFLG=0
C
      DO 100 I=1,NISPA
        IF(ISPA(1,I) .EQ. ISID) THEN
          CALL ADDSET(I,ISPA,IS,NN)
          IADDFLG=1
          GOTO 200
        ENDIF
  100 CONTINUE
C
  200 IF(IADDFLG .EQ. 1) THEN
C
        DO 300 I=1,NN
          DO 310 J=1,NISP1
            IF(ISP1(1,J) .EQ. NSPA(IS-1+I)) THEN
              IF(IDYN .EQ. 1 .AND. ISP1(1,J) .GE. IGNR) GOTO 330
              CALL ADDSET(J,ISP1,IS1,NN1)
              DO 311 K=1,NN1
                IAD=IS1-1+K
                NODFIX=NSP1(1,IAD)
                DO 312 L=1,6
                  IF(NSP1(L+1,IAD).EQ.1) INDOF(L,NODFIX)=1
  312           CONTINUE
  311         CONTINUE
              GOTO 330
            ENDIF
  310     CONTINUE
C
  330     DO 320 J=1,NISPC
            IF(ISPC(1,J) .EQ. NSPA(IS-1+I)) THEN
              IF(IDYN .EQ. 1 .AND. ISPC(1,J) .GE. IGNR) GOTO 300
              CALL ADDSET(J,ISPC,ISC,NNC)
              DO 321 K=1,NNC
                IAD=ISC-1+K
                NODFIX=NSPC(1,IAD)
                DO 322 L=1,6
                  IF(NSPC(L+1,IAD).EQ.1) THEN
                    IF(IMODE.EQ.1) THEN
                      INDOF(L,NODFIX)=1
                    ELSE
                      INDOF(L,NODFIX)=-1
                      UG(L,NODFIX)=SPC(L,IAD)
                    ENDIF
                  ENDIF
  322           CONTINUE
  321         CONTINUE
              GOTO 300
            ENDIF
  320     CONTINUE
  300   CONTINUE
C
      ELSE
C
        DO 410 J=1,NISP1
          IF(ISP1(1,J) .EQ. ISID) THEN
            IF(IDYN .EQ. 1 .AND. ISP1(1,J) .GE. IGNR) GOTO 430
            CALL ADDSET(J,ISP1,IS1,NN1)
            DO 411 K=1,NN1
              IAD=IS1-1+K
              NODFIX=NSP1(1,IAD)
              DO 412 L=1,6
                IF(NSP1(L+1,IAD).EQ.1) INDOF(L,NODFIX)=1
  412         CONTINUE
  411       CONTINUE
            GOTO 430
          ENDIF
  410   CONTINUE
C
  430   DO 420 J=1,NISPC
          IF(ISPC(1,J) .EQ. ISID) THEN
            IF(IDYN .EQ. 1 .AND. ISPC(1,J) .GE. IGNR) GOTO 500
            CALL ADDSET(J,ISPC,ISC,NNC)
            DO 421 K=1,NNC
              IAD=ISC-1+K
              NODFIX=NSPC(1,IAD)
              DO 422 L=1,6
                IF(NSPC(L+1,IAD).EQ.1) THEN
                  IF(IMODE.EQ.1) THEN
                    INDOF(L,NODFIX)=1
                  ELSE
                    INDOF(L,NODFIX)=-1
                    UG(L,NODFIX)=SPC(L,IAD)
                  ENDIF
                ENDIF
  422         CONTINUE
  421       CONTINUE
            GOTO 500
          ENDIF
  420   CONTINUE
C
      ENDIF
C
  500 CALL CLEAR0(IWK,NNOD)
      DO 700 IENO=1,NELM
        ITYP=IELM(2,IENO)
        ND  =IELM(3,IENO)
        IF(ITYP .EQ. 1 .AND. ND .EQ. 9) ND=8
        IF( ITYP .EQ. 1 .OR. ITYP .EQ. 4 ) THEN
          DO 710 I=1,ND
            NODE=IELM(7+I,IENO)
            IWK(NODE)=1
  710     CONTINUE
        ENDIF
  700 CONTINUE
C
      DO 600 I=1,NNOD
        IF( IWK(I) .EQ. 0 ) THEN
          DO 610 J=4,6
            INDOF(J,I)=1
  610     CONTINUE
        ENDIF
  600 CONTINUE
C
      DEALLOCATE( IWK )
C
      RETURN
      END
