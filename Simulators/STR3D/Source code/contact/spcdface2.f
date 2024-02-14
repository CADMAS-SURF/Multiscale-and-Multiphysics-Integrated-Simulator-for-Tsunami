      SUBROUTINE SPCDFACE2(P,IC,XYZ,RL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IC(3),X(5),Y(5),Z(5),XYZ(3,4),P(3),RL(3)
C----&------------------------------------------------------------------
C
C     IFR1 : FREE
C     IFR2 : SLAVE
C     IFR3 : CONSTRAINT
C
      DO 100 I=1,3
        IF( IC(I) .EQ. 0 ) THEN
          IFR1=I
        ELSEIF( IC(I) .EQ. 2 ) THEN
          IFR2=I
        ELSEIF( IC(I) .EQ. 1 ) THEN
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
      X(5) = RL(1)*XYZ(IFR1,1) + RL(2)*XYZ(IFR1,2) + RL(3)*XYZ(IFR1,3)
      Y(5) = RL(1)*XYZ(IFR2,1) + RL(2)*XYZ(IFR2,2) + RL(3)*XYZ(IFR2,3)
      Z(5) = RL(1)*XYZ(IFR3,1) + RL(2)*XYZ(IFR3,2) + RL(3)*XYZ(IFR3,3)
C
      R  = (X(1)-X(3))*(Z(2)-Z(3)) - (X(2)-X(3))*(Z(1)-Z(3))
      A1 = (Z(2)-Z(3))/R
      B1 = -( X(3)*(Z(2)-Z(3)) + (X(2)-X(3))*(Z(4)-Z(3)) ) / R
C
      R  = (X(2)-X(3))*(Z(1)-Z(3)) - (X(1)-X(3))*(Z(2)-Z(3))
      A2 = (Z(1)-Z(3))/R
      B2 = -( X(3)*(Z(1)-Z(3)) + (X(1)-X(3))*(Z(4)-Z(3)) ) / R
C
      A = (Y(1)-Y(3))*A1 + (Y(2)-Y(3))*A2
      B = (Y(1)-Y(3))*B1 + (Y(2)-Y(3))*B2 + Y(3)
C
      P(IFR1) = ( -A*B + X(5) + A*Y(5) ) / ( A*A + 1. )
      P(IFR2) = A*P(IFR1) + B
      P(IFR3) = Z(4)
C
      RETURN
      END
