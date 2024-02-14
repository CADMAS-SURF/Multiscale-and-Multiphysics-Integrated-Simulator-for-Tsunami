      SUBROUTINE VF_P1SUMD(DSEND,DRECV)

CD=== 概要 ===========================================================

CDT   VF_P1SUMD:データの総和を求める/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DSEND : IN  : R*8 : 送信するデータ
CD    DRECV : OUT : R*8 : データの総和

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

C     -- 総和をとる --
      IF (NPROCS.NE.1) THEN
        CALL VF_ZXMP_ALLSMD(DSEND,DRECV,IERR)
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
