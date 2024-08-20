      SUBROUTINE SF_C_MPI_SEND_D(DSEND,N,IDEST)

      USE MOD_COMM, ONLY: COMM_2FC_STR

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'

      DIMENSION DSEND(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_SEND(DSEND,N,MPI_DOUBLE_PRECISION,IDEST,1,COMM_2FC_STR
     &             ,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
