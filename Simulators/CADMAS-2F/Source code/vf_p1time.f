      SUBROUTINE VF_P1TIME(PT)

CD=== 概要 ===========================================================

CDT   VF_P1TIME:経過時間を得る

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C==== 実行 ===========================================================

C     -- 経過時間を得る --
      PT=0.0D0
      CALL VF_ZXMP_WTIME(PT)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
