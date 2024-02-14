      SUBROUTINE SF_MPI_SEND_D(DSEND,N,IDEST)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION DSEND(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_SEND(DSEND,N,MPI_DOUBLE_PRECISION,IDEST,1,MGCOMM,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
