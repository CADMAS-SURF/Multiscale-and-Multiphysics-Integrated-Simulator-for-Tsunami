      SUBROUTINE QIP2D(A,B,S,INDP)
C======================================================================
C     内積を計算する
C     S = (A,B)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(IN)::A(MX,MY),B(MX,MY)
      REAL(8),INTENT(OUT)::S
      INTEGER,INTENT(IN)::INDP(MX,MY)
C
      REAL(8)::Q,R,S0,SS
      INTEGER::I,J
C
      S = 0.0D0
      Q = 0.0D0
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J).GT.0 ) THEN
            S0 = S
            SS = A(I,J) * B(I,J)
            S = S0+SS
            R = S-S0
            Q = Q+(SS-R)
         END IF
  100 CONTINUE
      S = S+Q
C
      RETURN
      END
