      SUBROUTINE VF_ZXMG_INIT(IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_INIT:MPIマスク/初期化する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    IERR : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
CCC      CALL MPI_INIT(IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
