      SUBROUTINE MKPORS_AIR(GVA,GXA,GYA,GZA,INDPA,INDUA,INDVA,INDWA,KFA)
C======================================================================
C     風塲のポロシティを初期化する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
C
      REAL(8),INTENT(OUT)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(OUT)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
C      INTEGER::I,J,K
C
C
C ... 配列を0.0で初期化
      CALL ZERCLR(GVA,MXY*MZA,0.0D0)
      CALL ZERCLR(GXA,MXY*MZA,0.0D0)
      CALL ZERCLR(GYA,MXY*MZA,0.0D0)
      CALL ZERCLR(GZA,MXY*MZA,0.0D0)
C
      RETURN
      END
