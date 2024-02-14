      SUBROUTINE GREEN(EPS,DUDX,IGNL)
C
C     EPS  : OUT : Green-Lagrange歪
C                  EPS(1) = ε11
C                  EPS(2) = ε22
C                  EPS(3) = ε33
C                  EPS(4) = 2*ε12
C                  EPS(5) = 2*ε23
C                  EPS(6) = 2*ε31
C     DUDX : IN  : DUDX(i,j) = ∂uj/∂xi
C     IGNL : IN  : =0:微小変形, =1:大変形
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPS(6),DUDX(3,3)
C
      EPS(1)=DUDX(1,1)
      EPS(2)=DUDX(2,2)
      EPS(3)=DUDX(3,3)
      EPS(4)=DUDX(2,1)+DUDX(1,2)
      EPS(5)=DUDX(3,2)+DUDX(2,3)
      EPS(6)=DUDX(1,3)+DUDX(3,1)
C
      IF( IGNL == 0 ) RETURN
C
      EPS(1)=EPS(1)+0.5D0*( DUDX(1,1)*DUDX(1,1)
     &                    + DUDX(1,2)*DUDX(1,2)
     &                    + DUDX(1,3)*DUDX(1,3) )
      EPS(2)=EPS(2)+0.5D0*( DUDX(2,1)*DUDX(2,1)
     &                    + DUDX(2,2)*DUDX(2,2)
     &                    + DUDX(2,3)*DUDX(2,3) )
      EPS(3)=EPS(3)+0.5D0*( DUDX(3,1)*DUDX(3,1)
     &                    + DUDX(3,2)*DUDX(3,2)
     &                    + DUDX(3,3)*DUDX(3,3) )
      EPS(4)=EPS(4)+( DUDX(1,1)*DUDX(2,1)
     &              + DUDX(1,2)*DUDX(2,2)
     &              + DUDX(1,3)*DUDX(2,3) )
      EPS(5)=EPS(5)+( DUDX(2,1)*DUDX(3,1)
     &              + DUDX(2,2)*DUDX(3,2)
     &              + DUDX(2,3)*DUDX(3,3) )
      EPS(6)=EPS(6)+( DUDX(3,1)*DUDX(1,1)
     &              + DUDX(3,2)*DUDX(1,2)
     &              + DUDX(3,3)*DUDX(1,3) )
C
      END
