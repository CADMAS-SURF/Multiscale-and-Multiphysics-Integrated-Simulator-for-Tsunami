      SUBROUTINE VF_ZXMP_WAIT(IREQ,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_WAIT:MPIマスク/リクエストが終了するまで待つ(MGと同じ)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    IREQ    : I/O : I*4 : リクエスト
CD    IERR    : OUT : I*4 : 完了コード

CD    -- 局所変数 --
      DIMENSION ISTT(MPI_STATUS_SIZE)

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_WAIT(IREQ,ISTT,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
