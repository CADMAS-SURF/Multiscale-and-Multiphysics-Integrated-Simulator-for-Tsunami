      SUBROUTINE SPCDRHV(FT,ESTF,IDCR,NCR,INDOF,UG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION UG(6,*),ESTF(*),IDCR(2,NCR),INDOF(6,*),FT(*)
C-----------------------------------------------------------------------
      DO 100 J=1,NCR
        JJ=INDOF(IDCR(2,J),IDCR(1,J))
        IF(JJ .EQ. -1) THEN
          DO 200 I=1,NCR
            II=INDOF(IDCR(2,I),IDCR(1,I))
            IF(II .GT. 0) THEN
              IF(J.GE.I) THEN
                IAD=(2*NCR-I+2)*(I-1)/2+J-I+1
              ELSE
                IAD=(2*NCR-J+2)*(J-1)/2+I-J+1
              ENDIF
              FT(II)=FT(II)-ESTF(IAD)*UG(IDCR(2,J),IDCR(1,J))
            ENDIF
  200     CONTINUE
        ENDIF
  100 CONTINUE
C
      RETURN
      END
