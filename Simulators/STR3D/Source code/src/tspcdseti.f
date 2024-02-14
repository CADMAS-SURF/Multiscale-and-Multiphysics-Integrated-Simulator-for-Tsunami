      SUBROUTINE TSPCDSETI(INDOF,IDLOAD,KK,IDLD,LIDL,ITL1,ISPD,NSPD
     &                    ,IDYN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),INDOF(6,*),IDLD(2,*),LIDL(*),ITL1(4,*),ISPD(2,*)
     &         ,NSPD(7,*)
C-----------------------------------------------------------------------
      NDLD  = KK(66)
      NTLD1 = KK(77)
C
      IDLFLG=0
C
      DO 100 I=1,NDLD
        IF( IDLD(1,I) .EQ. IDLOAD ) THEN
          CALL ADDSET(I,IDLD,IS,NN)
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
C
          DO 310 J=1,NTLD1
C
            ID=ITL1(1,J)
            ITYPE=ITL1(3,J)
C
            IF( IDYN == 0 .AND. ITYPE >= 2 ) CYCLE
C
            IF( ID .EQ. LI .AND. ITYPE .GT. 0 ) THEN
              IEX=ITL1(2,J)
              CALL SPCDSETI(IEX,INDOF,KK,ISPD,NSPD)
              GOTO 300
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
          IF( IDYN == 0 .AND. ITYPE >= 2 ) CYCLE
C
          IF( ID .EQ. IDLOAD .AND. ITYPE .GT. 0 ) THEN
            IEX=ITL1(2,J)
            CALL SPCDSETI(IEX,INDOF,KK,ISPD,NSPD)
            GOTO 500
          ENDIF
C
  410   CONTINUE
C
      ENDIF
C
  500 RETURN
      END
