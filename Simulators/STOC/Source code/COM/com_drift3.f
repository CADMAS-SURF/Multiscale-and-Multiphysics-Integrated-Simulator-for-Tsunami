C=======================================================================
      SUBROUTINE COM_DRIFT3 (TIME,TEND)
C=======================================================================
C
C     時間積分計算の当該ステップの時刻の通信
C
C------------------------------------------------------------------------
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
      INCLUDE 'DRIFT.h'
C
      REAL(8),INTENT(INOUT)::TIME,TEND
C
      REAL(8)::AWK(2)
      INTEGER::IERR
      INTEGER::IREQ1
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::ITAG
C
C======================================================================
C
      IF( NOCALDM ) RETURN
C
      AWK(1) = TIME
      AWK(2) = TEND
      ITAG=280
      CALL MPI_ISEND(AWK,2,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                     comm_mlicds_dm,IREQ1,IERR)    !  時刻の通信
      CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
C-----------------------------------------------------------------------
      RETURN
      END
