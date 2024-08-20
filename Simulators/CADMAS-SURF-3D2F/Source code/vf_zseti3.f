      SUBROUTINE VF_ZSETI3(IA,IVAL,N1,N2,N3)

CD=== 概要 ===========================================================

CDT   VF_ZSETI3:整数の3次元配列に一定値を代入する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    IA(N1,N2,N3) : OUT : I*4 : 1次元配列
CD    IVAL         : IN  : I*4 : 一定値
CD    N1           : IN  : I*4 : 配列IAの第1サイズ
CD    N2           : IN  : I*4 : 配列IAの第2サイズ
CD    N3           : IN  : I*4 : 配列IAの第3サイズ
      DIMENSION IA(N1,N2,N3)

C==== 実行 ===========================================================

CD    -- 配列の全要素に代入 --
      DO 120 K=1,N3
        DO 110 J=1,N2
          DO 100 I=1,N1
            IA(I,J,K)=IVAL
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
