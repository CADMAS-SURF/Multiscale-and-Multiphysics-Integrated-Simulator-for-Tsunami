      SUBROUTINE CORRFACE2(DUG,IC,XYZ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IC(3),X(4),Y(4),Z(4),XYZ(3,4),DUG(3)
C----&------------------------------------------------------------------
C
C     IFR1 : FREE
C     IFR2 : CONSTRAINT
C     IFR3 : SLAVE
C
      DO 100 I=1,3
        IF( IC(I) .EQ. 0 ) THEN
          IFR1=I
        ELSEIF( IC(I) .EQ. 1 ) THEN
          IFR2=I
        ELSEIF( IC(I) .EQ. 2 ) THEN
          IFR3=I
        ENDIF
  100 CONTINUE
C
      DO 200 I=1,4
        X(I)=XYZ(IFR1,I)
        Y(I)=XYZ(IFR2,I)
        Z(I)=XYZ(IFR3,I)
  200 CONTINUE
C
      R  = (X(1)-X(3))*(Y(2)-Y(3)) - (X(2)-X(3))*(Y(1)-Y(3))
      A1 = (Y(2)-Y(3))/R
      B1 = ( (X(4)-X(3))*(Y(2)-Y(3)) - (X(2)-X(3))*(Y(4)-Y(3)) ) / R
C
      R  = (X(2)-X(3))*(Y(1)-Y(3)) - (X(1)-X(3))*(Y(2)-Y(3))
      A2 = (Y(1)-Y(3))/R
      B2 = ( (X(4)-X(3))*(Y(1)-Y(3)) - (X(1)-X(3))*(Y(4)-Y(3)) ) / R
C
      A = (Z(1)-Z(3))*A1 + (Z(2)-Z(3))*A2
      B = (Z(1)-Z(3))*B1 + (Z(2)-Z(3))*B2 + (Z(3)-Z(4))
C
      DUG(IFR1) = -A*B/(A*A+1.)
      DUG(IFR2) = 0.
      DUG(IFR3) = B/(A*A+1.)
C
      RETURN
      END
