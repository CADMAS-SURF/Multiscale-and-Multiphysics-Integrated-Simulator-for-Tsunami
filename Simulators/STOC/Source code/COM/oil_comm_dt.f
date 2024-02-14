      SUBROUTINE OIL_COMM_DT
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
      INTEGER :: IERR
C
C-----------------------------------------------------------------------
C
      IF(ICAL_OIL.NE.0 .AND. NRANK.EQ.0 .AND. IDT.GT.0 ) THEN
         CALL MPI_SEND(DT,1,MPI_DOUBLE_PRECISION,NP_OIL,0
     &                ,comm_mlicds_oil,IERR)
      END IF
C
      RETURN
      END
