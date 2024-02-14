      SUBROUTINE CP_NEIBREV(DUV,IDIR)
C======================================================================
C     IDIR = 1  :  DU(1,J,K) -> DU(MXM,J,K)
C     IDIR = 2  :  DV(I,1,K) -> DV(I,MYM,K)
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
C
      REAL(8),INTENT(INOUT)::DUV(MX,MY,MZ)
      INTEGER,INTENT(IN)::IDIR
C
      REAL(8)::BF1(MX,MZ),BF2(MY,MZ)
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::I,IERROR,IREQ,ITAG,J,K
      INTEGER::NDATA1,NDATA2,NEIE,NEIN,NEIS,NEIW
C
      ITAG=0
      NDATA1=MX*MZ
      NDATA2=MY*MZ
      NEIS=IPECON(4,NRANK+1)
      NEIW=IPECON(5,NRANK+1)
      NEIE=IPECON(6,NRANK+1)
      NEIN=IPECON(7,NRANK+1)
C
      IF(IDIR.EQ.2) THEN
C
      IF(NEIS.GE.0) THEN
        DO 110 K=1,MZ
        DO 110 I=1,MX
          BF1(I,K)=DUV(I,1,K)
  110   CONTINUE
        CALL MPI_ISEND(BF1,NDATA1,MPI_DOUBLE_PRECISION,NEIS,
     &                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(NEIN.GE.0) THEN
        CALL MPI_IRECV(BF1,NDATA1,MPI_DOUBLE_PRECISION,NEIN,
     &                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 120 K=1,MZ
        DO 120 I=1,MX
          DUV(I,MYM,K)=DUV(I,MYM,K)+BF1(I,K)
  120   CONTINUE
      END IF
C
      ELSE IF(IDIR.EQ.1) THEN
C
      IF(NEIW.GE.0) THEN
        DO 130 K=1,MZ
        DO 130 J=1,MY
          BF2(J,K)=DUV(1,J,K)
  130   CONTINUE
        CALL MPI_ISEND(BF2,NDATA2,MPI_DOUBLE_PRECISION,NEIW,
     &                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(NEIE.GE.0) THEN
        CALL MPI_IRECV(BF2,NDATA2,MPI_DOUBLE_PRECISION,NEIE,
     &                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 140 K=1,MZ
        DO 140 J=1,MY
          DUV(MXM,J,K)=DUV(MXM,J,K)+BF2(J,K)
  140   CONTINUE
      END IF
C
      END IF
C
      RETURN
      END
