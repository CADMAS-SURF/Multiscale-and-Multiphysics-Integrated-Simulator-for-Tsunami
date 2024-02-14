      SUBROUTINE VF_ZXMP_BCASTC(CBUF,N,IROOT,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_BCASTC:MPIマスク/irootからその他へ送信する/文字

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    CBUF  : I/O : C*(*) : 送信・受信するデータ
CD    N     : IN  : I*4   : データの要素数
CD    IROOT : IN  : I*4   : 送信元のランク
CD    IERR  : OUT : I*4   : 完了コード
      CHARACTER*(*) CBUF

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_BCAST(CBUF,N,MPI_CHARACTER,IROOT,MGCOMM,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
