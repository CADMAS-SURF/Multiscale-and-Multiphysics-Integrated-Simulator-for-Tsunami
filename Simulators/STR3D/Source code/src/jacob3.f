      SUBROUTINE JACOB3(NG,P,XYZ,XJI,DET,ITO)
C
C     NG  : IN  : 節点数
C     P   : IN  : P(i,j) = ∂Nj/∂ξi
C     XYZ : IN  : 節点座標
C     XJI : OUT : [J] ( XJ(I,J) = ∂xj/∂ξi ) の逆行列
C     DET : OUT : |J|
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XJ(3,3),XJI(3,3),P(3,NG),XYZ(3,NG)
C
      DO 100 I=1,3
        DO 200 J=1,3
          XJ(I,J)=0.
          DO 300 K=1,NG
            XJ(I,J)=XJ(I,J)+P(I,K)*XYZ(J,K)
  300     CONTINUE
  200   CONTINUE
  100 CONTINUE
C
      CALL INV3(XJ,XJI,DET,ITO)
C
      IF( DET .LT. 1.D-20 )  THEN
          WRITE(ITO,*) 'ELEMENT VOLUME IS TOO SMALL.'
          CALL ERRSTP(11,ITO)
      ENDIF
C
      RETURN
      END
