      SUBROUTINE COM_STOC3(TIME,TEND)
C----------------------------------------
C     時刻の通信
C----------------------------------------
      USE M_COM_STOC,ONLY: IP_STOC_MAIN
      USE M_OUTPUT,ONLY:IFL
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      REAL(8),INTENT(INOUT)::TIME,TEND
C
      REAL(8)::AWK(2)
      INTEGER::IREQ1
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::IERR
C
C
      CALL MPI_IRECV(AWK,2,MPI_DOUBLE_PRECISION,IP_STOC_MAIN,
     &                 MPI_ANY_TAG,comm_mlicds_dm,IREQ1,IERR)    !  時刻の通信
      CALL MPI_WAIT(IREQ1,ISTAT,IERR)
      TIME = AWK(1)
      TEND = AWK(2)
C
C      IF ( MOD(INT(10000.0*(TIME+1.0D-5)),100000) .EQ. 0 ) THEN
C         WRITE (IFL,'(A20,F12.5)') '      TIME [s]  =   ',TIME
C      ENDIF
C
C
      RETURN
      END
