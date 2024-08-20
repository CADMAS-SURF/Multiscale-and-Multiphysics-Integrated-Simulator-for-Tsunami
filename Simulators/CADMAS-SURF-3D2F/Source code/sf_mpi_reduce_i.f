      SUBROUTINE SF_MPI_REDUCE_I(ISEND,IRECV,N,IROOT)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION ISEND(N),IRECV(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      IF( NPROCS > 1 ) THEN
        CALL MPI_REDUCE(ISEND,IRECV,N,MPI_INTEGER,MPI_SUM,IROOT,MGCOMM
     &                 ,IERR)
      ELSE
        IRECV = ISEND
      ENDIF

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
