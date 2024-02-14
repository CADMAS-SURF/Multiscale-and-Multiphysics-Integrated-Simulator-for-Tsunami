      SUBROUTINE AREACD2(RL,X)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(2,4),RL(3)

      X1 = X(1,1)
      Y1 = X(2,1)
      X2 = X(1,2)
      Y2 = X(2,2)
      X3 = X(1,3)
      Y3 = X(2,3)
      X4 = X(1,4)
      Y4 = X(2,4)

      S2 = ( X2*Y3 + X1*Y2 + X3*Y1 ) - ( X2*Y1 + X1*Y3 + X3*Y2 )

      A1 = X2*Y3 - X3*Y2
      B1 = Y2 - Y3
      C1 = X3 - X2

      A2 = X3*Y1 - X1*Y3
      B2 = Y3 - Y1
      C2 = X1 - X3

      A3 = X1*Y2 - X2*Y1
      B3 = Y1 - Y2
      C3 = X2 - X1

      RL(1) = ( A1 + B1*X4 + C1*Y4 ) / S2
      RL(2) = ( A2 + B2*X4 + C2*Y4 ) / S2
      RL(3) = ( A3 + B3*X4 + C3*Y4 ) / S2

      END
