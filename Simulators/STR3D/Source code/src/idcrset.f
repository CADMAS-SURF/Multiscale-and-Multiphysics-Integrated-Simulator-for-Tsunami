      SUBROUTINE IDCRSET(IMPC,KN,ND,NDF,INDOF,INDMPC,MPCF,IDCR,NCR)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(ND),IDCR(2,*),INDMPC(2,6,*),MPCF(2,*),INDOF(6,*)
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
      IRAMFLG=0
      DO 300 I=1,ND
        DO 310 J=1,NDF
          IF(INDOF(J,KN(I)) .NE. -2) GOTO 310
          IRAMFLG=1
          IS=INDMPC(1,J,KN(I))
          IE=INDMPC(2,J,KN(I))
C
          DO 320 IAD=IS,IE
            NODE=MPCF(1,IAD)
            IFR =MPCF(2,IAD)
            DO 330 K=1,NCR
              IF(NODE .EQ. IDCR(1,K) .AND. IFR .EQ. IDCR(2,K)) GOTO 320
  330       CONTINUE
            NCR=NCR+1
            IDCR(1,NCR)=NODE
            IDCR(2,NCR)=IFR
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
            NCR=NCR-1
          ELSE
            IP=IP+1
          ENDIF
  410   CONTINUE
  400 CONTINUE
C
  900 RETURN
      END
