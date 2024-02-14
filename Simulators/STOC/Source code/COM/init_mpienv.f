      SUBROUTINE INIT_MPIENV(IERR)
C----------------------------------------------------------------------
C     MPI環境の初期化とコミュニケータの分割を行う
C----------------------------------------------------------------------
      use mod_comm,only: nsize_all,l_model,l_stoc_ml,l_stoc_ic
     $                  ,l_stoc_dm,l_stoc_oil,comm_model,comm_group
     $                  ,comm_mlicds_dm,comm_work_mlicds_dm
     $                  ,comm_mlicds_oil,comm_work_mlicds_oil
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'DRIFT.h'
      INCLUDE 'OIL.h'
C
      INTEGER,INTENT(OUT):: IERR
      INTEGER:: N,IB_SD,IREQ,ISTAT(MPI_STATUS_SIZE),N2,N3
C
      CALL MPI_COMM_SIZE(comm_model,NSIZEALL,IERR)
      CALL MPI_COMM_RANK(comm_model,NRANK,IERR)
C
      NSIZE=0
      N2=0
      N3=0
      NB_SD=-1
      NP_OIL=-1
      DO N=0,nsize_all-1
         IF( l_model(N).EQ.l_stoc_ml.or.
     $       l_model(N).EQ.l_stoc_ic ) then
            NSIZE=NSIZE+1
            N2=N2+1
            N3=N3+1
         ELSE IF( l_model(N).EQ.l_stoc_dm  ) then
            NB_SD=N2   ! NB_SDの設定(DRIFT連成)
            N2=N2+1
         ELSE IF( l_model(N).EQ.l_stoc_oil ) then
            NP_OIL=N3   ! NP_OILの設定(OIL連成)
            N3=N3+1
         ENDIF
      ENDDO
C
C
C
C
C
C
C
C ... その他の特殊処理
C     DRIFT用
      comm_mlicds_dm = comm_work_mlicds_dm
      NB_SD_MAIN=0
      IF( NB_SD.GE.0 ) THEN ! DMとの連成時
C
         CALL MPI_IRECV(IB_SD,1,MPI_INTEGER,NB_SD,
     $                  MPI_ANY_TAG,comm_mlicds_dm,IREQ,IERR )
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
         IF( IB_SD.EQ.2 ) THEN
            NB_SD_MAIN=1
         ENDIF
         CALL MPI_IRECV(NOCALDM,1,MPI_LOGICAL,NB_SD,
     $                  MPI_ANY_TAG,comm_mlicds_dm,IREQ,IERR )
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
         CALL MPI_IRECV(OFF_INTERVAL,1,MPI_DOUBLE_PRECISION,NB_SD,
     $                  MPI_ANY_TAG,comm_mlicds_dm,IREQ,IERR )
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
         CALL MPI_IRECV(OFF_START,1,MPI_DOUBLE_PRECISION,NB_SD,
     $                  MPI_ANY_TAG,comm_mlicds_dm,IREQ,IERR )
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         IF( IB_SD.EQ.0 ) THEN
            NB_SD=-1 ! DMと連成しない領域からはDMが見えないようにする
         ENDIF
      ENDIF
C
C ... OIL用
      ICAL_OIL=0
      IF( NP_OIL.GE.0 ) THEN ! OILとの連成時
         comm_mlicds_oil = comm_work_mlicds_oil
         CALL MPI_ALLREDUCE(0,ICAL_OIL,1,MPI_INTEGER,MPI_MAX
     &                                  ,comm_mlicds_oil,IERR)
      END IF
C
      RETURN
      END
