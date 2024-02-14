      SUBROUTINE SCATTER_NODAL_D(X,NDF)

      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(NDF,*)

      REAL(8), POINTER :: W(:,:)

      MN = MAXVAL( NN_EXT )

      ALLOCATE( W(NDF,MN) )

      DO IP = 1, NPROCS - 1
        N = NN_EXT(IP)
        W(:,1:N) = X(:,NOD(1:N,IP))
        CALL M_MPI_SEND_D(W,NDF*N,IP)
      ENDDO

      DEALLOCATE( W )

      END
