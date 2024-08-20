      SUBROUTINE VF_STOC_INIT(IERR)

CD=== 概要 ===========================================================

CDT   VF_STOC_INIT:STOCとの通信環境を初期化する

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: nrank_all,comm_work_ic_mg,comm_ic_mg,my_model,
     $                   l_cadmas_2fc
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE  'mpif.h'
      include 'VF_ASTOCI.h'
      include 'VF_ASTOCR.h'
C ... WORK VARIABLES
      INTEGER IERR,M,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
      INTEGER IB_STOC0,ITMP,IWORK0,IWORK(512)

CD    -- 引数 --
CD    IERR : OUT : I*4 : 完了コード

C==== 実行 ===========================================================

C     << STOC<-->CADMAS 通信用変数の設定 >>
C     IB_STOC, NB_CADMAS, IB_CADMASの3種を設定する
C
C ... 初期化
      IB_STOC = -1
      NB_CADMAS = 0
      LB_CADMAS = 0
      DO N=1,MAX_CADMAS
         IB_CADMAS(N) = -1
      ENDDO
      IWORK0 = -1
C
      if( my_model.eq.l_cadmas_2fc ) then
         NB_SC=0
         return
      endif
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
      NB_CADMAS = ITMP-1
C
C
C     <<< IB_STOCの設定 >>>
      IB_STOC0 = -1
      CALL MPI_ALLREDUCE(IB_STOC0,IB_STOC,1,MPI_INTEGER,MPI_MAX,
     $                   comm_ic_mg,IERR)
C
C
C     <<< IB_CADMASの設定 >>>
      IWORK0 = IRANK
      CALL MPI_ALLGATHER(IWORK0,1,MPI_INTEGER,
     $                   IWORK,1,MPI_INTEGER,comm_ic_mg,IERR)
C
      M = 0
      DO N=1,NB_CADMAS+1
         IF( IWORK(N).GE.0 ) THEN
            M = M + 1
            IB_CADMAS(M) = IWORK(N)
            IF( IRANK.EQ.IWORK(N) ) LB_CADMAS = M
         ENDIF
      ENDDO
CDEBUG      write(*,*) 'ib_stoc=0=',ib_stoc
CDEBUG      write(*,*) 'nb_cadmas=1=',nb_cadmas
CDEBUG      write(*,*) 'lb_cadmas=1=',lb_cadmas
CDEBUG      write(*,*) 'ib_cadmas=1=',ib_cadmas(1:nb_cadmas)
CDEBUG      write(*,*) 'nb_sc=1=',nb_sc
C
C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
