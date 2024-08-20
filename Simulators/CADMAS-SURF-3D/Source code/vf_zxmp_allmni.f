      SUBROUTINE VF_ZXMP_ALLMNI(ISEND,IRECV,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_ALLMNI:MPIマスク/データの最小値を求める/整数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISEND : IN  : R*8 : 送信するデータ
CD    IRECV : OUT : R*8 : データの最小値
CD    IERR  : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_ALLREDUCE(ISEND,IRECV,1,MPI_INTEGER,MPI_MIN,
     &                   MGCOMM,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
