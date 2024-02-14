      SUBROUTINE INIT_MPIENV(IERR)
C----------------------------------------------------------------------
C     MPI環境の初期化とコミュニケータの分割を行う
C----------------------------------------------------------------------
      USE M_COM_STOC,ONLY: NSIZEALL,NSIZE,NRANK
      use mod_comm,only: nsize_all,l_model,l_stoc_ml,l_stoc_ic
     $                  ,comm_work_mlicds_dm,comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      INTEGER,INTENT(OUT):: IERR
CDEBUG      INTEGER:: N
C
CCC      CALL MPI_INIT(IERR)
      comm_mlicds_dm=comm_work_mlicds_dm
      CALL MPI_COMM_SIZE(comm_mlicds_dm,NSIZEALL,IERR)
      CALL MPI_COMM_RANK(comm_mlicds_dm,NRANK,IERR)
C
      NSIZE=NSIZEALL-1
CDEBUG      DO N=0,nsize_all-1
CDEBUG         IF( l_model(N).eq.l_stoc_ml.or.
CDEBUG     $       l_model(N).eq.l_stoc_ic.or.
CDEBUG     $       l_model(N).eq.l_stoc_ds ) THEN
CDEBUG            NSIZE=NSIZE+1
CDEBUG         ENDIF
CDEBUG      ENDDO
C
      RETURN
      END
