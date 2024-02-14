      SUBROUTINE VF_ZXMG_ALLMND(DSEND,DRECV,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_ALLMND:MPIマスク/データの最小値を求める/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_model
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    DSEND : IN  : R*8 : 送信するデータ
CD    DRECV : OUT : R*8 : データの最小値
CD    IERR  : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_ALLREDUCE(DSEND,DRECV,1,MPI_DOUBLE_PRECISION,MPI_MIN,
     &                   comm_model,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
