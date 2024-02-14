      SUBROUTINE CP_KFEXPND(KFD,KF)
C======================================================================
C     KFを拡げる
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
C
      INTEGER,INTENT(INOUT)::KFD(MX+1,MY+1),KF(MX,MY)
C
      INTEGER::KFX(MX),KFY(MY)
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::I,J,ITAG,IREQ,IERROR,NEIS,NEIR
C
      DO 100 J=1,MY
      DO 100 I=1,MX
        KFD(I,J) = KF(I,J)
  100 CONTINUE
C
      ITAG = 0
      IF(IPECON(4,NRANK+1).GE.0) THEN
        DO 110 I=1,MX
          KFX(I) = KF(I,3)
  110   CONTINUE
        NEIS = IPECON(4,NRANK+1)
        CALL MPI_ISEND(KFX,MX,MPI_INTEGER,NEIS,
     *                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(7,NRANK+1).GE.0) THEN
        NEIR = IPECON(7,NRANK+1)
        CALL MPI_IRECV(KFX,MX,MPI_INTEGER,NEIR,
     *                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 120 I=1,MX
          KFD(I,MY+1) = KFX(I)
  120   CONTINUE
      END IF
C
      IF(IPECON(5,NRANK+1).GE.0) THEN
        DO 130 J=1,MY
          KFY(J) = KF(3,J)
  130   CONTINUE
        NEIS = IPECON(5,NRANK+1)
        CALL MPI_ISEND(KFY,MY,MPI_INTEGER,NEIS,
     *                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(6,NRANK+1).GE.0) THEN
        NEIR = IPECON(6,NRANK+1)
        CALL MPI_IRECV(KFY,MY,MPI_INTEGER,NEIR,
     *                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 140 J=1,MY
          KFD(MX+1,J) = KFY(J)
  140   CONTINUE
      END IF
C
      RETURN
      END
