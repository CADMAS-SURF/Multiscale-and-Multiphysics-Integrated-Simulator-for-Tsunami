      SUBROUTINE SEND_KK(KK,N)

      DIMENSION KK(*)

      CALL M_MPI_SEND_I(1,1,0)  ! SEND IOP=1 TO GLB_COMM
      CALL M_MPI_SEND_I(N,1,0)
      CALL M_MPI_SEND_I(KK(N),1,0)

      END
