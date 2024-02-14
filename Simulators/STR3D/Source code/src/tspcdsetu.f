      SUBROUTINE TSPCDSETU(DUG,IDLOAD,KK,IDLD,SDLD,SIDL,LIDL,ITL1,ITD1
     &                    ,TBD1,ISPD,NSPD,SPCD,UG1,UG2,TIM3,DT1,DT2
     &                    ,IDYN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),IDLD(2,*),SDLD(*),SIDL(*),LIDL(*),ITL1(4,*)
     &         ,ISPD(2,*),NSPD(7,*),SPCD(6,*),ITD1(*),TBD1(2,*),UG1(6,*)
     &         ,UG2(6,*),DUG(6,*)
C-----------------------------------------------------------------------
      NDLD  = KK(66)
      NTLD1 = KK(77)
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
            IF( IDYN .EQ. 0 .AND. ITYPE .GE. 2 ) CYCLE
C
            IF( ID .EQ. LI .AND. ITYPE .GT. 0 ) THEN
C
              IEX=ITL1(2,J)
C
              IF( ITYPE .EQ. 1 ) THEN
                TIM=TIM3
              ELSE
                TIM=TIM3-DT2
              ENDIF
C
              ITF=ITL1(4,J)
              CALL ADDSET2(ITF,ITD1,IP,N)
C
              CALL INTPL(FAC,TIM,TBD1(1,IP),N)
              FAC=FAC*S*SI
C
              CALL SPCDSETDU(IEX,DUG,KK,ISPD,NSPD,SPCD,FAC,ITYPE,UG1,UG2
     &                      ,DT1,DT2)
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
          IF( IDYN .EQ. 0 .AND. ITYPE .GE. 2 ) CYCLE
C
          IF( ID .EQ. IDLOAD .AND. ITYPE .GT. 0 ) THEN
C
            IEX=ITL1(2,J)
C
            IF( ITYPE .EQ. 1 ) THEN
              TIM=TIM3
            ELSE
              TIM=TIM3-DT2
            ENDIF
C
            ITF=ITL1(4,J)
            CALL ADDSET2(ITF,ITD1,IP,N)
C
            CALL INTPL(FAC,TIM,TBD1(1,IP),N)
C
            CALL SPCDSETDU(IEX,DUG,KK,ISPD,NSPD,SPCD,FAC,ITYPE,UG1,UG2
     &                    ,DT1,DT2)
C
            GOTO 500
C
          ENDIF
C
  410   CONTINUE
C
      ENDIF
C
  500 RETURN
      END
