      SUBROUTINE CP_DSR_FFF(FF)
C======================================================================
C     FFの追加通信
C     FF(1,MY,*)s=FF(1,2,*)n,FF(MX,1,*)w=FF(2,1,*)e
C     FF(MX,MY,*)w=FF(2,MY,*)e
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
C
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
C
      REAL(8)::FFZ(MZ)
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::K,ITAG,IREQ,IERROR,NEIS,NEIR,IFL
C
      ITAG = 0
      IFL = 0
C
      IF(IPECON(4,NRANK+1).GE.0) THEN
        DO 100 K=1,MZ
          FFZ(K) = FF(1,2,K)
  100   CONTINUE
        NEIS = IPECON(4,NRANK+1)
        CALL MPI_ISEND(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIS,
     *                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(7,NRANK+1).GE.0) THEN
        NEIR = IPECON(7,NRANK+1)
        CALL MPI_IRECV(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIR,
     *                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 120 K=1,MZ
          FF(1,MY,K) = FFZ(K)
  120   CONTINUE
      END IF
C
      IF(IPECON(5,NRANK+1).GE.0) THEN
        DO 140 K=1,MZ
          FFZ(K) = FF(2,1,K)
  140   CONTINUE
        NEIS = IPECON(5,NRANK+1)
        CALL MPI_ISEND(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIS,
     *                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
C
        DO 150 K=1,MZ
          FFZ(K) = FF(2,MY,K)
 150    CONTINUE
        CALL MPI_ISEND(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIS,
     *                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
C
      END IF
C
      IF(IPECON(6,NRANK+1).GE.0) THEN
        NEIR = IPECON(6,NRANK+1)
        CALL MPI_IRECV(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIR,
     *                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 160 K=1,MZ
          FF(MX,1,K) = FFZ(K)
  160   CONTINUE
C
        CALL MPI_IRECV(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIR,
     *                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 170 K=1,MZ
          FF(MX,MY,K) = FFZ(K)
 170    CONTINUE
      END IF
C
      IF(IPECON(4,NRANK+1).GE.0.AND.IPECON(6,NRANK+1).LT.0) THEN
        DO 180 K=1,MZ
          FFZ(K) = FF(MX,2,K)
  180   CONTINUE
        NEIS = IPECON(4,NRANK+1)
        CALL MPI_ISEND(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIS,
     *                 ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(7,NRANK+1).GE.0.AND.IPECON(6,NRANK+1).LT.0) THEN
        NEIR = IPECON(7,NRANK+1)
        CALL MPI_IRECV(FFZ,MZ,MPI_DOUBLE_PRECISION,NEIR,
     *                 MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        DO 190 K=1,MZ
          FF(MX,MY,K) = FFZ(K)
 190    CONTINUE
      END IF
C
      RETURN
      END
