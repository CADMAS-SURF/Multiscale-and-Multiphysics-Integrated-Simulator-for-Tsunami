      SUBROUTINE VF_P1INIT()

CD=== 概要 ===========================================================

CDT   VF_P1INIT:並列環境を初期化する

C==== 宣言 ===========================================================

      use mod_comm,only: comm_model,comm_work_2fc_dem,comm_2fc_dem
      use mod_dem

C     -- 大域型 --

      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'
      INCLUDE  'mpif.h'

C==== 実行 ===========================================================

C     -- VF_APARAI.hを初期化する --


C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
