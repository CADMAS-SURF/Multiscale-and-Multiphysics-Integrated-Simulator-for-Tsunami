      SUBROUTINE SF_MPI_ISEND_I(ISEND,N,IDEST,IREQ)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION ISEND(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_ISEND(ISEND,N,MPI_INTEGER,IDEST,1,MGCOMM,IREQ,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
