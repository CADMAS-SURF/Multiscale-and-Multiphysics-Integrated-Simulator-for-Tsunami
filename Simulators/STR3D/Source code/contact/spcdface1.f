      SUBROUTINE SPCDFACE1(P,XYZ,RL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3),RL(3),XYZ(3,3)
C----&------------------------------------------------------------------
      DO J=1,3
        P(J) = RL(1)*XYZ(J,1) + RL(2)*XYZ(J,2) + RL(3)*XYZ(J,3)
      ENDDO
C
      RETURN
      END
