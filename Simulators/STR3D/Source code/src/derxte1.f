      SUBROUTINE DERXTE1(DNDX,V6,XYZ,ITO)
C
C     DNDX : OUT : DNDX(i,j) = ∂Nj/∂xi
C     V6   : OUT : テトラの体積*6
C     XYZ  : IN  : 節点座標
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),B(4),C(4),D(4),DNDX(3,4)
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
      V6 = -(   (X1-X4)*(Y2-Y4)*(Z3-Z4) + (X3-X4)*(Z2-Z4)*(Y1-Y4)
     &        + (X2-X4)*(Y3-Y4)*(Z1-Z4) - (Z1-Z4)*(Y2-Y4)*(X3-X4)
     &        - (X1-X4)*(Y3-Y4)*(Z2-Z4) - (Y1-Y4)*(X2-X4)*(Z3-Z4)  )
C
      B(1) = -( Y3*Z4 + Y4*Z2 + Z3*Y2 - Z2*Y3 - Y2*Z4 - Y4*Z3 ) / V6
      C(1) = -( X2*Z4 + X3*Z2 + X4*Z3 - Z2*X4 - X3*Z4 - X2*Z3 ) / V6
      D(1) = -( X2*Y3 + X3*Y4 + X4*Y2 - Y3*X4 - Y2*X3 - X2*Y4 ) / V6
C
      B(2) = -( Y1*Z4 + Y4*Z3 + Z1*Y3 - Z3*Y1 - Y3*Z4 - Y4*Z1 ) / V6
      C(2) = -( X3*Z4 + X1*Z3 + X4*Z1 - Z3*X4 - X1*Z4 - X3*Z1 ) / V6
      D(2) = -( X3*Y1 + X1*Y4 + X4*Y3 - Y1*X4 - Y3*X1 - X3*Y4 ) / V6
C
      B(3) = -( Y1*Z2 + Y2*Z4 + Z1*Y4 - Z4*Y1 - Y4*Z2 - Y2*Z1 ) / V6
      C(3) = -( X4*Z2 + X1*Z4 + X2*Z1 - Z4*X2 - X1*Z2 - X4*Z1 ) / V6
      D(3) = -( X4*Y1 + X1*Y2 + X2*Y4 - Y1*X2 - Y4*X1 - X4*Y2 ) / V6
C
      B(4) = -( Y3*Z2 + Y2*Z1 + Z3*Y1 - Z1*Y3 - Y1*Z2 - Y2*Z3 ) / V6
      C(4) = -( X1*Z2 + X3*Z1 + X2*Z3 - Z1*X2 - X3*Z2 - X1*Z3 ) / V6
      D(4) = -( X1*Y3 + X3*Y2 + X2*Y1 - Y3*X2 - Y1*X3 - X1*Y2 ) / V6
C
      DO 100 J=1,4
        DNDX(1,J)=B(J)
        DNDX(2,J)=C(J)
        DNDX(3,J)=D(J)
  100 CONTINUE
C
      IF( V6 < 1.D-20 ) THEN
        WRITE(ITO,*) 'ELEMENT VOLUME IS TOO SMALL.'
        CALL ERRSTP(11,ITO)
      ENDIF
C
      RETURN
      END
