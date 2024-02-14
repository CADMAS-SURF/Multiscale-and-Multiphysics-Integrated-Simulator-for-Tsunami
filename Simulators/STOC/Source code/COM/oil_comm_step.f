      SUBROUTINE OIL_COMM_STEP
C
      use mod_comm,only: comm_mlicds_oil
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'OIL.h'
      INCLUDE 'mpif.h'
C
      INTEGER :: IBUF(3)
      REAL(8) :: RBUF(3)
      INTEGER :: IERR
C
C-----------------------------------------------------------------------
C
      IF(ICAL_OIL.NE.0 .AND. NRANK.EQ.0) THEN
        IBUF(1) = ISTEP+1
        IBUF(2) = MAXSTP
        IBUF(3) = IDT
        CALL MPI_SEND(IBUF,3,MPI_INTEGER,NP_OIL,0,comm_mlicds_oil,IERR)
        RBUF(1) = TIME
        RBUF(2) = REND
        RBUF(3) = DT
        CALL MPI_SEND(RBUF,3,MPI_DOUBLE_PRECISION,NP_OIL,0
     &                      ,comm_mlicds_oil,IERR)
      END IF
C
      RETURN
      END

