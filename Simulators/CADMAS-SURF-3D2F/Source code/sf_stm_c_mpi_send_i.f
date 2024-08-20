      SUBROUTINE SF_STM_C_MPI_SEND_I(ISEND,N,IDEST)

      USE MOD_COMM, ONLY: COMM_2FC_STM

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'

      DIMENSION ISEND(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_SEND(ISEND,N,MPI_INTEGER,IDEST,1,COMM_2FC_STM
     &             ,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
