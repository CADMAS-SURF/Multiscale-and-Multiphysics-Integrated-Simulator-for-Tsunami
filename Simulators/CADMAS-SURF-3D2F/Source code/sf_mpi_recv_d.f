      SUBROUTINE SF_MPI_RECV_D(DRECV,N,ISRC)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION DRECV(N),ISTATUS(MPI_STATUS_SIZE)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_RECV(DRECV,N,MPI_DOUBLE_PRECISION,ISRC,1,MGCOMM,ISTATUS
     &             ,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
