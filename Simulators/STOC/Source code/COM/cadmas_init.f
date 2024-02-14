      SUBROUTINE CADMAS_INIT
C----------------------------------------------------------------------
C     CADMASとの接続及び通信に関する変数を設定する
C     IB_STOC, NB_CADMAS, IB_CADMAS
C----------------------------------------------------------------------
      use mod_comm,only: nrank_all,comm_work_ic_mg,comm_ic_mg
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AREA.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CADMAS.h'
C
C ... WORK VARIABLES
      INTEGER IERR,M,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
      INTEGER IB_STOC0,ITMP,IWORK0,IWORK(MAX_STOC+MAX_CADMAS)
      INTEGER ISIZ1,ISIZ2
C
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
      ISIZ1 = 1
      CALL MPI_ALLREDUCE(ISIZ1,ISIZ2,1,MPI_INTEGER
     $                   ,mpi_sum,comm_ic_mg,ierr)
      NB_STOC   = ISIZ2
      NB_CADMAS = ITMP-NB_STOC
      ITAGSC    = NB_STOC*NB_CADMAS
C
C
C     <<< IB_STOCの設定 >>>
      IWORK0 = IRANK
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
      IWORK0 = -1
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
C
C ... CADMASとの境界を速度固定境界として設定
      NOBSS = 1
      NINLT = 4
      NAREA = 4
C
      RETURN
      END
