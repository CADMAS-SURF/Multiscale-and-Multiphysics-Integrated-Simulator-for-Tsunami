      SUBROUTINE NCRSET(ND,NDF,KN,INDOF,INDMPC,MPCF,IDWK,NCRMAX,NRAMAX)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION INDOF(6,*),KN(ND),INDMPC(2,6,*),MPCF(2,*),IDWK(2,*)
C-----------------------------------------------------------------------
      NC=ND*NDF
      NCR=0
C
      DO 300 I=1,ND
        DO 310 J=1,NDF
          IF(INDOF(J,KN(I)) .NE. -2) GOTO 310
C+++      NCR=NCR-1
          IS=INDMPC(1,J,KN(I))
          IE=INDMPC(2,J,KN(I))
C
          DO 320 IAD=IS,IE
            NODE=MPCF(1,IAD)
            IFR =MPCF(2,IAD)
            DO 330 K=1,ND
              IF(NODE .EQ. KN(K)) GOTO 320
  330       CONTINUE
            DO 340 K=1,NCR
              IF(NODE .EQ. IDWK(1,K) .AND. IFR .EQ. IDWK(2,K)) GOTO 320
  340       CONTINUE
            NCR=NCR+1
            IDWK(1,NCR)=NODE
            IDWK(2,NCR)=IFR
  320     CONTINUE
C
  310   CONTINUE
  300 CONTINUE
C
      NCR=NC+NCR
C+++  NCR=MAX0(NC,NCR)
      NRA=NC*NCR
      NCRMAX=MAX0(NCRMAX,NCR)
      NRAMAX=MAX0(NRAMAX,NRA)
C     
      RETURN
      END
