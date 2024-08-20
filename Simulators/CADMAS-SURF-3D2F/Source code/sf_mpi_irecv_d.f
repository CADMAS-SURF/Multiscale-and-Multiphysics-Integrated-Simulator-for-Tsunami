      SUBROUTINE SF_MPI_IRECV_D(DRECV,N,ISRC,IREQ)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION DRECV(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_IRECV(DRECV,N,MPI_DOUBLE_PRECISION,ISRC,1,MGCOMM,IREQ
     &              ,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
