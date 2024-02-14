      SUBROUTINE MPCTRNS(IMPC,KN,ND,NDF,NC,RAMBDA,NRAMAX,INDOF,INDMPC
     &                  ,MPCF,RMPC,WK,ESTF,IDCR,NCR)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(ND),IDCR(2,*),RAMBDA(NC,*),INDOF(6,*),INDMPC(2,6,*)
     &         ,MPCF(2,*),RMPC(*),WK(NC,*),ESTF(*)
C-----------------------------------------------------------------------
      NCR=0
      DO 100 I=1,ND
      DO 100 J=1,NDF
        NCR=NCR+1
        IDCR(1,NCR)=KN(I)
        IDCR(2,NCR)=J
  100 CONTINUE
C
      IF(IMPC .EQ. 0) GOTO 900
C
      CALL CLEAR1(RAMBDA,NRAMAX)
      DO 200 I=1,NC
        RAMBDA(I,I)=1.D0
  200 CONTINUE
C
      IRAMFLG=0
      DO 300 I=1,ND
        DO 310 J=1,NDF
          IF(INDOF(J,KN(I)) .NE. -2) GOTO 310
          IRAMFLG=1
          IROW=NDF*(I-1)+J
          IS=INDMPC(1,J,KN(I))
          IE=INDMPC(2,J,KN(I))
C
          DO 320 IAD=IS,IE
            NODE=MPCF(1,IAD)
            IFR =MPCF(2,IAD)
            DO 330 K=1,NCR
              IF(NODE .EQ. IDCR(1,K) .AND. IFR .EQ. IDCR(2,K)) THEN
                RAMBDA(IROW,K)=RMPC(IAD)
                GOTO 320
              ENDIF
  330       CONTINUE
            NCR=NCR+1
            IDCR(1,NCR)=NODE
            IDCR(2,NCR)=IFR
            RAMBDA(IROW,NCR)=RMPC(IAD)
  320     CONTINUE
C
  310   CONTINUE
  300 CONTINUE
C     
      IF(IRAMFLG .EQ. 0) GOTO 900
C
      IP=1
      DO 400 I=1,ND
        DO 410 J=1,NDF
          IF(INDOF(J,KN(I)) .EQ. -2) THEN
            CALL SHIFT0(IDCR(1,IP),IDCR(1,IP+1),2*(NCR-IP))
            CALL SHIFT1(RAMBDA(1,IP),RAMBDA(1,IP+1),NC*(NCR-IP))
            NCR=NCR-1
          ELSE
            IP=IP+1
          ENDIF
  410   CONTINUE
  400 CONTINUE
C
      CALL HARFB(WK,ESTF,RAMBDA,NC,NC,NCR)
      CALL HARFTA(ESTF,RAMBDA,WK,NC,NCR)
C
  900 RETURN
      END
