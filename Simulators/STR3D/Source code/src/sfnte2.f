      SUBROUTINE SFNTE2(RL,H)
C
C     RL : IN  : 体積座標
C     H  : OUT : H(i) = Ni
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION H(10),RL(4)
C
      H( 1) = RL(1)*(2.*RL(1)-1.) 
      H( 2) = RL(2)*(2.*RL(2)-1.)
      H( 3) = RL(3)*(2.*RL(3)-1.)
      H( 4) = RL(4)*(2.*RL(4)-1.)
      H( 5) = 4.*RL(1)*RL(2)
      H( 6) = 4.*RL(2)*RL(3)
      H( 7) = 4.*RL(1)*RL(3)
      H( 8) = 4.*RL(1)*RL(4)
      H( 9) = 4.*RL(2)*RL(4)
      H(10) = 4.*RL(3)*RL(4)
C
      RETURN
      END
