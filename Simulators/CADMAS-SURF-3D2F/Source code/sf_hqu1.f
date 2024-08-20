      SUBROUTINE SF_HQU1(H,XL,YL,X,Y)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION H(4)

      R = 2.D0 * X / XL - 1.D0
      S = 2.D0 * Y / YL - 1.D0

      H(1) = .25D0 * ( 1.D0 - R ) * ( 1.D0 - S )
      H(2) = .25D0 * ( 1.D0 + R ) * ( 1.D0 - S )
      H(3) = .25D0 * ( 1.D0 + R ) * ( 1.D0 + S )
      H(4) = .25D0 * ( 1.D0 - R ) * ( 1.D0 + S )

      END
