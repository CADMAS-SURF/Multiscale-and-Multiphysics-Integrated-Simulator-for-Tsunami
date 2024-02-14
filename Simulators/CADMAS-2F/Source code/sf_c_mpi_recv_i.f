      SUBROUTINE SF_C_MPI_RECV_I(IRECV,N,ISRC)

      USE MOD_COMM, ONLY: COMM_2FC_STR

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'

      DIMENSION IRECV(N),ISTATUS(MPI_STATUS_SIZE)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_RECV(IRECV,N,MPI_INTEGER,ISRC,1,COMM_2FC_STR
     &             ,ISTATUS,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
