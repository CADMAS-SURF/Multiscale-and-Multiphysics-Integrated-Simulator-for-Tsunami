      SUBROUTINE VF_ZSETI2(IA,IVAL,N1,N2)

CD=== 概要 ===========================================================

CDT   VF_ZSETI2:整数の2次元配列に一定値を代入する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    IA(N1,N2) : OUT : I*4 : 1次元配列
CD    IVAL      : IN  : I*4 : 一定値
CD    N1        : IN  : I*4 : 配列IAの第1サイズ
CD    N2        : IN  : I*4 : 配列IAの第2サイズ
      DIMENSION IA(N1,N2)

C==== 実行 ===========================================================

CD    -- 配列の全要素に代入 --
      DO 110 J=1,N2
        DO 100 I=1,N1
          IA(I,J)=IVAL
 100    CONTINUE
 110  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
