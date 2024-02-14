      SUBROUTINE ZERCLR(RR,NN,VALUE)
C ... 配列に一定値を設定する
C     通常、0.0D0でクリアするために使用する。
C
      IMPLICIT NONE
C
      INTEGER,INTENT(INOUT)::NN
      REAL(8),INTENT(INOUT)::RR(NN),VALUE
      INTEGER::N
C
      DO 100 N=1,NN
         RR(N) = VALUE
  100 CONTINUE
C
      RETURN
      END
