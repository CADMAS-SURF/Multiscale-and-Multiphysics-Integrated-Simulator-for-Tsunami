      SUBROUTINE CHKCNS(SN,SUMSC)
C======================================================================
C     セルに含まれるスカラー量の合計を計算する
C       SN: セルのスカラー量×セルの水体積
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::SN(MX,MY,MZ)
      REAL(8),INTENT(OUT)::SUMSC
C
      REAL(8)::SUM1,DSUM
C
      INTEGER::I,J,K
C
C
      SUMSC = 0.0D0
      DSUM  = 0.0D0
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         SUM1  = SUMSC
         SUMSC = (DSUM+SN(I,J,K)) + SUM1
         DSUM  = ( SUMSC - SUM1 ) - SN(I,J,K)
 100  CONTINUE
C
      RETURN
      END
