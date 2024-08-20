      SUBROUTINE VF_ZXMG_CRANK(MGRANK,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_CRANK:MPIマスク/自分のランクを得る

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_model
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    MGRANK : OUT : I*4 : 自分のランク
CD    IERR   : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_COMM_RANK(comm_model,MGRANK,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
