      SUBROUTINE ZERCLI(II,NN,IVALUE)
C ... 配列に一定値を設定する
C     通常、0でクリアするために使用する。
C
      IMPLICIT NONE
C
      INTEGER,INTENT(INOUT)::NN
      INTEGER,INTENT(INOUT)::II(NN),IVALUE
      INTEGER::N
C
      DO 100 N=1,NN
         II(N) = IVALUE
  100 CONTINUE
C
      RETURN
      END
