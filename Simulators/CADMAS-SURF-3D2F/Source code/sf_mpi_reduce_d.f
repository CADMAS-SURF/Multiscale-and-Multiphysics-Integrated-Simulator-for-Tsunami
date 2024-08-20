      SUBROUTINE SF_MPI_REDUCE_D(DSEND,DRECV,N,IROOT)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION DSEND(N),DRECV(N)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      IF( NPROCS > 1 ) THEN
        CALL MPI_REDUCE(DSEND,DRECV,N,MPI_DOUBLE_PRECISION,MPI_SUM,IROOT
     &                 ,MGCOMM,IERR)
      ELSE
        DRECV = DSEND
      ENDIF

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
