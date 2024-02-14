      SUBROUTINE DERTR2(RL,P,ND)
C
C     RL : IN  : 面積座標
C     P  : OUT : P(i,j) = ∂Nj/∂ξi
C     ND : IN  : 節点数
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(2,ND),RL(3)
C
      P( 1, 1) = 4.*RL(1)-1.
      P( 1, 2) = 0.
      P( 1, 3) = -(4.*RL(3)-1.)
      P( 1, 4) = 4.*RL(2)
      P( 1, 5) = -4.*RL(2)
      P( 1, 6) = 4.*(RL(3)-RL(1))
C
      P( 2, 1) = 0.
      P( 2, 2) = 4.*RL(2)-1.
      P( 2, 3) = -(4.*RL(3)-1.)
      P( 2, 4) = 4.*RL(1)
      P( 2, 5) = 4.*(RL(3)-RL(2))
      P( 2, 6) = -4.*RL(1)
C
      IF( ND .EQ. 7 ) THEN
        P(1,7) = 27.*RL(2)*(RL(3)-RL(1))
        P(2,7) = 27.*RL(1)*(RL(3)-RL(2))
        DO I = 1, 6
          DO J = 1, 2
            IF( I .LE. 3 ) THEN
              FAC = 1.D0/3.D0
            ELSE
              FAC = -2.D0/3.D0
            ENDIF
            P(J,I) = P(J,I) + FAC*P(J,7)
          ENDDO
        ENDDO
      ENDIF
C
      RETURN
      END
