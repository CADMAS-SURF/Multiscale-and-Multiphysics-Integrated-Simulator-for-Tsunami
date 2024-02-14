      SUBROUTINE DBEAM0(D,E,RNU)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(6)

      G = .5D0 * E / ( 1.D0 + RNU )

      D(:) = 0.

      D(1) = E
      D(4) = G
      D(6) = G

      END