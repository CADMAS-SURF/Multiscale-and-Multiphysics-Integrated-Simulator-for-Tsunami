      SUBROUTINE CORRFACE1(DUG,XYZ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),RL(3),RN(3),DUG(3)
C----&------------------------------------------------------------------
      CALL AREACD(RL,RN,H,XYZ)
      CALL RMULT1(DUG,RN,-H,3)
C
      RETURN
      END
