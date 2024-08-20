      SUBROUTINE VF_STOC_INIT(IERR)

CD=== 概要 ===========================================================

CDT   VF_STOC_INIT:STOCとの通信環境を初期化する

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: nrank_all,comm_work_ic_mg,comm_ic_mg
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'
      include 'VF_ASTOCI.h'
      include 'VF_ASTOCR.h'
C ... WORK VARIABLES
      INTEGER IERR,M,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
      INTEGER IB_STOC0,ITMP,IWORK0,IWORK(MAX_STOC+MAX_CADMAS)
      INTEGER ISIZ1,ISIZ2

CD    -- 引数 --
CD    IERR : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     << STOC<-->CADMAS 通信用変数の設定 >>
C     IB_STOC, NB_CADMAS, IB_CADMASの3種を設定する
C
C ... 初期化
      NB_STOC   = 0
      LB_STOC   = 0
      DO N=1,MAX_STOC
         IB_STOC(N) = -1
      ENDDO
      NB_CADMAS = 0
      LB_CADMAS = 0
      DO N=1,MAX_CADMAS
         IB_CADMAS(N) = -1
      ENDDO
      IWORK0 = -1
C
C ... 同じNB_SC値をもつSTOC <--> CADMAS通信のグループを一時的に作成
      CALL MPI_COMM_SPLIT(comm_work_ic_mg,NB_SC,NRANK_ALL,comm_ic_mg,
     $                    IERR)
C
C ... STOC-CADMAS連成に関らないPEはRETURN
      IF( NB_SC.EQ.0 ) RETURN
C
C
C     <<< NB_CADMASの設定 >>>
      CALL MPI_COMM_SIZE(comm_ic_mg,ITMP,IERR)
      CALL MPI_COMM_RANK(comm_ic_mg,IRANK,IERR)
C
      ISIZ1 = 0
      CALL MPI_ALLREDUCE(ISIZ1,ISIZ2,1,MPI_INTEGER
     $                   ,mpi_sum,comm_ic_mg,ierr)
      NB_STOC   = ISIZ2
      NB_CADMAS = ITMP-NB_STOC
      ITAGSC    = NB_STOC*NB_CADMAS
C
C
C     <<< IB_STOCの設定 >>>
      IWORK0 = -1
      CALL MPI_ALLGATHER(IWORK0,1,MPI_INTEGER,
     $                   IWORK,1,MPI_INTEGER,comm_ic_mg,IERR)
C
      M = 0
cmod141022s
C      DO N=1,NB_STOC
      DO N=1,ITMP
cmod141022e
         IF( IWORK(N).GE.0 ) THEN
            M = M + 1
            IB_STOC(M) = IWORK(N)
            IF( IRANK.EQ.IWORK(N) ) LB_STOC = M
         ENDIF
      ENDDO
C
C
C     <<< IB_CADMASの設定 >>>
      IWORK0 = IRANK
      CALL MPI_ALLGATHER(IWORK0,1,MPI_INTEGER,
     $                   IWORK,1,MPI_INTEGER,comm_ic_mg,IERR)
C
      M = 0
      DO N=1,NB_CADMAS+NB_STOC
         IF( IWORK(N).GE.0 ) THEN
            M = M + 1
            IB_CADMAS(M) = IWORK(N)
            IF( IRANK.EQ.IWORK(N) ) LB_CADMAS = M
         ENDIF
      ENDDO
C
C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
