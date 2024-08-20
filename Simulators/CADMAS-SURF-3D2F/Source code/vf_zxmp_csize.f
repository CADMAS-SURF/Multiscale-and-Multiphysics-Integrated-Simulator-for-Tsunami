      SUBROUTINE VF_ZXMP_CSIZE(IOPROC,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_CSIZE:MPIマスク/プロセス数を得る

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    IOPROC : OUT : I*4 : プロセス数
CD    IERR   : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_COMM_SIZE(MGCOMM,IOPROC,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
