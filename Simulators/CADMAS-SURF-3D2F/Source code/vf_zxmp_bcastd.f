      SUBROUTINE VF_ZXMP_BCASTD(BUF,N,IROOT,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_BCASTD:MPIマスク/irootからその他へ送信する/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BUF(N)  : I/O : R*8 : 送信・受信するデータ
CD    N       : IN  : I*4 : データの要素数
CD    IROOT   : IN  : I*4 : 送信元のランク
CD    IERR    : OUT : I*4 : 完了コード
      DIMENSION BUF(N)

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_BCAST(BUF,N,MPI_DOUBLE_PRECISION,IROOT,MGCOMM,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
