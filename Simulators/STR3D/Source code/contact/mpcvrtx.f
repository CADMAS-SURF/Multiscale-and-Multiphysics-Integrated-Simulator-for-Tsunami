      SUBROUTINE MPCVRTX(IDEP,IDX,IRH,IS,NS,MA,IVRQ,IELQ,INDOF,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IDEP(2,3),IDX(2,3),IRH(2,*),IELQ(4,*),INDOF(3)
C----&------------------------------------------------------------------
      IF( INDOF(1).NE.0 .OR. INDOF(2).NE.0 .OR. INDOF(3).NE.0 ) THEN
        WRITE(ITO,'(/X,A)') 'SLAVE POINT IS CONSTRAINED.'
        CALL ERRSTP(50,ITO)
      ENDIF
C
      IF( IVRQ .EQ. 0 ) THEN
C
        DO 100 I=1,3
          IDEP(1,I)=NS
          IDEP(2,I)=I
          IDX(1,I)=IS+I-1
          IDX(2,I)=IS+I-1
          IRH(1,I)=MA
          IRH(2,I)=I
  100   CONTINUE
C
      ELSE
C
        IP=0
        DO 200 I=1,3
          IDEP(1,I)=NS
          IDEP(2,I)=I
          IDX(1,I)=IS+4*(I-1)
          IDX(2,I)=IS+4*I-1
          DO 210 J=1,4
            IP=IP+1
            IRH(1,IP)=IELQ(J,IVRQ)
            IRH(2,IP)=I
  210     CONTINUE
  200   CONTINUE
C
      ENDIF
C
      END
