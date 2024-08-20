      SUBROUTINE VF_ZSETR3(AA,VAL,N1,N2,N3)

CD=== 概要 ===========================================================

CDT   VF_ZSETR3:実数の3次元配列に一定値を代入する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    AA(N1,N2,N3) : OUT : R*8 : 2次元配列
CD    VAL          : IN  : R*8 : 一定値
CD    N1           : IN  : I*4 : 配列AAの第1サイズ
CD    N2           : IN  : I*4 : 配列AAの第2サイズ
CD    N3           : IN  : I*4 : 配列AAの第3サイズ
      DIMENSION AA(N1,N2,N3)

C==== 実行 ===========================================================

CD    -- 配列の全要素に代入 --
      DO 120 K=1,N3
        DO 110 J=1,N2
          DO 100 I=1,N1
            AA(I,J,K)=VAL
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
