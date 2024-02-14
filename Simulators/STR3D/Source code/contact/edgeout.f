      SUBROUTINE EDGEOUT(ISLV,DL,IEDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IEDG(6),ISLV(2)
C----&------------------------------------------------------------------
      IF(DL .GT. 0.) THEN
        IM=IEDG(2)
      ELSE
        IM=IEDG(1)
      ENDIF
C
      ISLV(1)=11
      ISLV(2)=IM
C
      RETURN
      END
