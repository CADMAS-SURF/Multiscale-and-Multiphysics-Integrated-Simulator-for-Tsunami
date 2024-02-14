      SUBROUTINE SCATTER_NODAL_I(K,NDF)

      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION K(NDF,*)

      INTEGER, POINTER :: KW(:,:)

      MN = MAXVAL( NN_EXT )

      ALLOCATE( KW(NDF,MN) )

      DO IP = 1, NPROCS - 1
        N = NN_EXT(IP)
        KW(:,1:N) = K(:,NOD(1:N,IP))
        CALL M_MPI_SEND_I(KW,NDF*N,IP)
      ENDDO

      DEALLOCATE( KW )

      END
