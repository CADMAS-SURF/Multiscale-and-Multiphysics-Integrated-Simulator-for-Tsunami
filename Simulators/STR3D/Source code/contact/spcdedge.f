      SUBROUTINE SPCDEDGE(P,IC,XYZ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IC(3),X(3),Y(3),Z(3),XYZ(3,3),P(3)
C----&------------------------------------------------------------------
C
C     IFR1 : CONSTRAINT
C     IFR2 : SLAVE
C     IFR3 : SLAVE
C
      IF( IC(1) .EQ. 1 ) THEN
        IFR1=1
        IFR2=2
        IFR3=3
      ELSEIF( IC(2) .EQ. 1 ) THEN
        IFR1=2
        IFR2=1
        IFR3=3
      ELSEIF( IC(3) .EQ. 1 ) THEN
        IFR1=3
        IFR2=1
        IFR3=2
      ENDIF
C
      DO 100 I=1,3
        X(I)=XYZ(IFR1,I)
        Y(I)=XYZ(IFR2,I)
        Z(I)=XYZ(IFR3,I)
  100 CONTINUE
C
      T=(X(3)-X(1))/(X(2)-X(1))
C
      P(IFR1) = X(3)
      P(IFR2) = Y(1) + T*( Y(2)-Y(1) )
      P(IFR3) = Z(1) + T*( Z(2)-Z(1) )
C
      RETURN
      END
