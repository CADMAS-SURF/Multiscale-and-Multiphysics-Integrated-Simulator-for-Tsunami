      SUBROUTINE RECV_RR(RR)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RR(*)

      CALL M_MPI_RECV_I(N,1,1)
      CALL M_MPI_RECV_D(X,1,1)
      RR(N) = X

      END
