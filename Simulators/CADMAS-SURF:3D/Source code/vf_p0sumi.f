      SUBROUTINE VF_P0SUMI(ISEND,IRECV)

CD=== 概要 ===========================================================

CDT   VF_P0SUMI:データの総和を求める/整数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISEND : IN  : I*4 : 送信するデータ
CD    IRECV : OUT : I*4 : データの総和

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

C     -- 総和をとる --
      IF (MGPROC.NE.1) THEN
        CALL VF_ZXMG_ALLSMI(ISEND,IRECV,IERR)
      ELSE
        IRECV=ISEND
      ENDIF

C     -- 実行文の終了 --
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
