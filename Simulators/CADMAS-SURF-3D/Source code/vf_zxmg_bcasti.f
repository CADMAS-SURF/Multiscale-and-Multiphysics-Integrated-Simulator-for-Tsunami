      SUBROUTINE VF_ZXMG_BCASTI(IBUF,N,IROOT,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMG_BCASTI:MPIマスク/irootからその他へ送信する/整数

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_model
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    IBUF(N) : I/O : I*4 : 送信・受信するデータ
CD    N       : IN  : I*4 : データの要素数
CD    IROOT   : IN  : I*4 : 送信元のランク
CD    IERR    : OUT : I*4 : 完了コード
      DIMENSION IBUF(N)

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_BCAST(IBUF,N,MPI_INTEGER,IROOT,comm_model,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
