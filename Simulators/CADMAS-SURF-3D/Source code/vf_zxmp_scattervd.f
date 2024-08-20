      SUBROUTINE VF_ZXMP_SCATTERVD(DSEND,NDIM,NDATA,NDISP,NP,
     $                             DRECV,ISIZE,IERR)

CD=== 概要 ===========================================================

CDT   VF_ZXMP_SCATTERVD: 可変長の実数型データを0番PEから配る

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

      INTEGER NDIM,NP
      DOUBLE PRECISION DSEND(NDIM)
      INTEGER NDATA(NP)
      INTEGER NDISP(NP)
      INTEGER ISIZE
      DOUBLE PRECISION DRECV(ISIZE)

C==== 実行 ===========================================================

C     -- MPIルーチン --
      CALL MPI_SCATTERV(DSEND,NDATA,NDISP,MPI_DOUBLE_PRECISION,
     &                 DRECV,ISIZE,MPI_DOUBLE_PRECISION,
     &                 0,MGCOMM,IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
