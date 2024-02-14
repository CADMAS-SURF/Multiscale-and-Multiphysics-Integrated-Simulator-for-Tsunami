      SUBROUTINE QAX(AD,AL,AU,XX,BB,INDP,MZ0)
C======================================================================
C     行列AとベクトルXの積を計算する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::AD(MX,MY,MZ0),AL(3,MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::AU(3,MX,MY,MZ0),XX(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::BB(MX,MY,MZ0)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ0)
      INTEGER,INTENT(IN)::MZ0
C
      INTEGER::I,J,K
C
      DO 100 K=2,MZ0-1
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            BB(I,J,K) = AL(1,I,J,K)*XX(I-1,J,K)
     $                + AL(2,I,J,K)*XX(I,J-1,K)
     $                + AL(3,I,J,K)*XX(I,J,K-1)
     $                + AU(1,I,J,K)*XX(I+1,J,K)
     $                + AU(2,I,J,K)*XX(I,J+1,K)
     $                + AU(3,I,J,K)*XX(I,J,K+1)
     $                + AD(I,J,K)*XX(I,J,K)
         END IF
  100 CONTINUE
C
      RETURN
      END
