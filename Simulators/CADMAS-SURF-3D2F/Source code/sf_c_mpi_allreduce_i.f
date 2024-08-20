      SUBROUTINE SF_C_MPI_ALLREDUCE_I(ISEND,IRECV,N)

      USE MOD_COMM, ONLY: COMM_2FC_STR

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'

      DIMENSION ISEND(N),IRECV(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_ALLREDUCE(ISEND,IRECV,N,MPI_INTEGER,MPI_SUM
     &                  ,COMM_2FC_STR,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
