      SUBROUTINE OIL_COMM_GRID(XC,YC,MX,MY)
C
      use mod_comm,only: comm_mlicds_oil
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'OIL.h'
      INCLUDE 'mpif.h'
C
      INTEGER,INTENT(IN) :: MX,MY
      REAL(8),INTENT(IN) :: XC(8,MX)
      REAL(8),INTENT(IN) :: YC(8,MY)
C
      CHARACTER(LEN=32) :: CFILENAME
      INTEGER :: ISTAT(MPI_STATUS_SIZE)
      INTEGER :: IREQ
      INTEGER :: ITAG
      INTEGER :: IERR
C
C-----------------------------------------------------------------------
C
      IF(ICAL_OIL.NE.0) THEN
        ITAG=NRANK
        CFILENAME = CFLNM(:IFLNM-4)
        CALL MPI_ISEND(CFILENAME,32,MPI_CHARACTER,NP_OIL,ITAG,
     $                 comm_mlicds_oil,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTAT,IERR)
        CALL MPI_ISEND(MX,1,MPI_INTEGER,NP_OIL,ITAG,
     $                 comm_mlicds_oil,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTAT,IERR)
        CALL MPI_ISEND(MY,1,MPI_INTEGER,NP_OIL,ITAG,
     $                 comm_mlicds_oil,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
        CALL MPI_ISEND(XC,8*MX,MPI_DOUBLE_PRECISION,NP_OIL,ITAG,
     $                 comm_mlicds_oil,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTAT,IERR)
        CALL MPI_ISEND(YC,8*MY,MPI_DOUBLE_PRECISION,NP_OIL,ITAG,
     $                 comm_mlicds_oil,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTAT,IERR)
      END IF
      IF(ICAL_OIL.EQ.2) THEN  ! OFFLINE
        CALL MPI_FINALIZE(IERR)
        STOP
      END IF
C
      RETURN
      END
