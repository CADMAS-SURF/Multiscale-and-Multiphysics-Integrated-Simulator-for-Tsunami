C=======================================================================
      SUBROUTINE COM_DRIFT4(IFLAG)
C=======================================================================
C
C     漂流計算との同期
C
C------------------------------------------------------------------------
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      INTEGER::IFLAG,ISEND,IRECV
      INTEGER::IERR
C
C======================================================================
C
      CALL MPI_BARRIER(comm_mlicds_dm,IERR)               !  漂流計算との同期
C
      ISEND = IFLAG                                       !  終了判定
      CALL MPI_ALLREDUCE(ISEND,IRECV,1,MPI_INTEGER,MPI_MAX,
     &                                              comm_mlicds_dm,IERR)
      IFLAG = IRECV
C
C-----------------------------------------------------------------------
      RETURN
      END
