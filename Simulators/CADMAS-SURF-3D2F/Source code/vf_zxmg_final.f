      SUBROUTINE VF_ZXMG_FINAL(IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_FINAL:MPIマスク/終了する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    IERR : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_FINALIZE(IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
