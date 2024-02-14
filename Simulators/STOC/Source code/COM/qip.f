      SUBROUTINE QIP(A,B,S,INDP,MZ0)
C======================================================================
C     内積を計算する
C     S = (A,B)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::A(MX,MY,MZ0),B(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::S
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ0)
      INTEGER,INTENT(IN)::MZ0
C
      REAL(8)::Q,R,S0,SS
      INTEGER::I,J,K
C
      S = 0.0D0
      Q = 0.0D0
      DO 100 K=2,MZ0-1
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            S0 = S
            SS = A(I,J,K) * B(I,J,K)
C            S = S0+SS+Q
            S = S0+SS
            R = S-S0
C            Q = SS+Q-R
            Q = Q+(SS-R)
         END IF
  100 CONTINUE
      S = S+Q
C
      RETURN
      END
