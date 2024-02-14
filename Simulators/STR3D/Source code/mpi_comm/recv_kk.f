      SUBROUTINE RECV_KK(KK)

      DIMENSION KK(*)

      CALL M_MPI_RECV_I(N,1,1)
      CALL M_MPI_RECV_I(KX,1,1)
      KK(N) = KX

      END
