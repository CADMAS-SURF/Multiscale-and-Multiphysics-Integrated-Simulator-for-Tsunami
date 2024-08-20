      SUBROUTINE SF_MPI_ALLREDUCE_I(ISEND,IRECV,N,IOP)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION ISEND(N),IRECV(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      SELECT CASE( IOP )
      CASE( 0 )
        MPI_OP = MPI_SUM
      CASE( 1 )
        MPI_OP = MPI_MAX
      CASE( 2 )
        MPI_OP = MPI_MIN
      END SELECT

      IF( NPROCS > 1 ) THEN
        CALL MPI_ALLREDUCE(ISEND,IRECV,N,MPI_INTEGER,MPI_OP,MGCOMM
     &                    ,IERR)
      ELSE
        IRECV = ISEND
      ENDIF

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
