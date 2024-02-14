      SUBROUTINE FACERL(RL,POSS,IELC,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RL(3),IELC(3),POS(3,*),XYZ(3,4),E(3),POSS(3)
C----&------------------------------------------------------------------
      CALL SHIFT1(XYZ(1,4),POSS,3)
      DO 100 I=1,3
        CALL SHIFT1(XYZ(1,I),POS(1,IELC(I)),3)
  100 CONTINUE
C
      CALL AREACD(RL,E,H,XYZ)
C
      RETURN
      END
