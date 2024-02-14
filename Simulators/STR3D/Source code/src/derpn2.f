      SUBROUTINE DERPN2(P,ND,R,S,T)
C
C     P  : OUT : P(i,j) = ∂Nj/∂ξi
C     ND : IN  : 節点数
C     R  : IN  : ξ (ξ1)
C     S  : IN  : η (ξ2)
C     T  : IN  : ζ (ξ3)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3,ND)
C
      R1 = .5D0 * R
      R2 = 2.D0 * R
C
      S1 = .5D0 * S
      S2 = 2.D0 * S
C
      A  = 1.D0 - T
      A1 = .5D0 * A
      A2 = 2.D0 * A
C
      B  = 1.D0 + T
      B1 = .5D0 * B
      B2 = 2.D0 * B
C
      C  = 1.D0 - R - S
      C1 = .5D0 * C
      C2 = 2.D0 * C
C
      P(1,1) = -A1
      P(2,1) = -A1
      P(3,1) = -C1
C
      P(1,2) = A1
      P(2,2) = 0.
      P(3,2) = -R1
C
      P(1,3) = 0.
      P(2,3) = A1
      P(3,3) = -S1
C
      P(1,4) = -B1
      P(2,4) = -B1
      P(3,4) = C1
C
      P(1,5) = B1
      P(2,5) = 0.
      P(3,5) = R1
C
      P(1,6) = 0.
      P(2,6) = B1
      P(3,6) = S1
C
      IF( ND == 6 ) RETURN
C
      D1 = R2 + S2 + B
      D2 = S2 + C2 + B
      D3 = R2 + C2 + B
      D4 = R2 + S2 + A
      D5 = S2 + C2 + A
      D6 = R2 + C2 + A
C
      D1 = 1.D0 - D1
      D2 = 1.D0 - D2
      D3 = 1.D0 - D3
      D4 = 1.D0 - D4
      D5 = 1.D0 - D5
      D6 = 1.D0 - D6
C
      P(1,1) = P(1,1) * ( D1 + C2 )
      P(2,1) = P(2,1) * ( D1 + C2 )
      P(3,1) = P(3,1) * ( D1 + A )
C
      P(1,2) = P(1,2) * ( D2 + R2 )
      P(2,2) = 0.
      P(3,2) = P(3,2) * ( D2 + A )
C
      P(1,3) = 0.
      P(2,3) = P(2,3) * ( D3 + S2 )
      P(3,3) = P(3,3) * ( D3 + A )
C
      P(1,4) = P(1,4) * ( D4 + C2 )
      P(2,4) = P(2,4) * ( D4 + C2 )
      P(3,4) = P(3,4) * ( D4 + B )
C
      P(1,5) = P(1,5) * ( D5 + R2 )
      P(2,5) = 0.
      P(3,5) = P(3,5) * ( D5 + B )
C
      P(1,6) = 0.
      P(2,6) = P(2,6) * ( D6 + S2 )
      P(3,6) = P(3,6) * ( D6 + B )
C
      P(1,7) = A2 * ( C - R )
      P(2,7) = -A2 * R
      P(3,7) = -C2 * R
C
      P(1,8) = A2 * S
      P(2,8) = A2 * R
      P(3,8) = -R2 * S
C
      P(1,9) = -A2 * S
      P(2,9) = A2 * ( C - S )
      P(3,9) = -C2 * S
C
      P(1,10) = -1.D0 + T*T
      P(2,10) = -1.D0 + T*T
      P(3,10) = -C2 * T
C
      P(1,11) = 1.D0 - T*T
      P(2,11) = 0.
      P(3,11) = -R2 * T
C
      P(1,12) = 0.
      P(2,12) = 1.D0 - T*T
      P(3,12) = -S2 * T
C
      P(1,13) = B2 * ( C - R )
      P(2,13) = -B2 * R
      P(3,13) = C2 * R
C
      P(1,14) = B2 * S
      P(2,14) = B2 * R
      P(3,14) = R2 * S
C
      P(1,15) = -B2 * S
      P(2,15) = B2 * ( C - S )
      P(3,15) = C2 * S
C
      END
