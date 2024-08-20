      SUBROUTINE SF_TETVOL(XYZ,V)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4)
C----&------------------------------------------------------------------
      X1 = XYZ(1,1)
      Y1 = XYZ(2,1)
      Z1 = XYZ(3,1)
      X2 = XYZ(1,2)
      Y2 = XYZ(2,2)
      Z2 = XYZ(3,2)
      X3 = XYZ(1,3)
      Y3 = XYZ(2,3)
      Z3 = XYZ(3,3)
      X4 = XYZ(1,4)
      Y4 = XYZ(2,4)
      Z4 = XYZ(3,4)
C
      V = -(   (X1-X4)*(Y2-Y4)*(Z3-Z4) + (X3-X4)*(Z2-Z4)*(Y1-Y4)
     &       + (X2-X4)*(Y3-Y4)*(Z1-Z4) - (Z1-Z4)*(Y2-Y4)*(X3-X4)
     &       - (X1-X4)*(Y3-Y4)*(Z2-Z4) - (Y1-Y4)*(X2-X4)*(Z3-Z4)  )/6.D0
C
C    IF( V < 0.D0 ) CALL VF_A2ERR('SF_TETVOL','P.G ERROR.')
C
      END
