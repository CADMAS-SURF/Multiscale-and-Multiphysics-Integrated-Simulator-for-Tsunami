      SUBROUTINE SPCDFACE3(P,IC,XYZ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IC(3),X(4),Y(4),Z(4),XYZ(3,4),P(3)
C----&------------------------------------------------------------------
C
C     IFR1 : CONSTRAINT
C     IFR2 : CONSTRAINT
C     IFR3 : SLAVE
C
      IF( IC(1) .EQ. 2 ) THEN
        IFR3=1
        IFR1=2
        IFR2=3
      ELSEIF( IC(2) .EQ. 2 ) THEN
        IFR3=2
        IFR1=1
        IFR2=3
      ELSEIF( IC(3) .EQ. 2 ) THEN
        IFR3=3
        IFR1=1
        IFR2=2
      ENDIF
C
      DO 200 I=1,4
        X(I)=XYZ(IFR1,I)
        Y(I)=XYZ(IFR2,I)
        Z(I)=XYZ(IFR3,I)
  200 CONTINUE
C
      R  = (X(1)-X(3))*(Y(2)-Y(3)) - (X(2)-X(3))*(Y(1)-Y(3))
      B1 = ( (X(4)-X(3))*(Y(2)-Y(3)) - (X(2)-X(3))*(Y(4)-Y(3)) ) / R
C
      R  = (X(2)-X(3))*(Y(1)-Y(3)) - (X(1)-X(3))*(Y(2)-Y(3))
      B2 = ( (X(4)-X(3))*(Y(1)-Y(3)) - (X(1)-X(3))*(Y(4)-Y(3)) ) / R
C
      P(IFR1) = X(4)
      P(IFR2) = Y(4)
      P(IFR3) = (Z(1)-Z(3))*B1 + (Z(2)-Z(3))*B2 + Z(3)
C
      RETURN
      END
