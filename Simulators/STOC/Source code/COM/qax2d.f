      SUBROUTINE QAX2D(AD,AL,AU,XX,BB,INDP)
C======================================================================
C     行列AとベクトルXの積を計算する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(IN)::AD(MX,MY),AL(2,MX,MY)
      REAL(8),INTENT(IN)::AU(2,MX,MY),XX(MX,MY)
      REAL(8),INTENT(OUT)::BB(MX,MY)
      INTEGER,INTENT(IN)::INDP(MX,MY)
C
      INTEGER::I,J
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J).GT.0 ) THEN
            BB(I,J) =   AL(1,I,J)*XX(I-1,J)
     $                + AL(2,I,J)*XX(I,J-1)
     $                + AU(1,I,J)*XX(I+1,J)
     $                + AU(2,I,J)*XX(I,J+1)
     $                + AD(I,J)*XX(I,J)
         END IF
  100 CONTINUE
C
      RETURN
      END
