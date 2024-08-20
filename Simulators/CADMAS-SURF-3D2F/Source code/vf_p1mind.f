      SUBROUTINE VF_P1MIND(DSEND,DRECV)

CD=== 概要 ===========================================================

CDT   VF_P1MIND:データの最小値を求める/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DSEND : IN  : R*8 : 送信するデータ
CD    DRECV : OUT : R*8 : データの最小値

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

C     -- 最小値をとる --
      IF (NPROCS.NE.1) THEN
        CALL VF_ZXMP_ALLMND(DSEND,DRECV,IERR)
      ELSE
        DRECV=DSEND
      ENDIF

C     -- 実行文の終了 --
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
