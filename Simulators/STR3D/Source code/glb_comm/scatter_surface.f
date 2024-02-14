      SUBROUTINE SCATTER_SURFACE(X,NDF)

      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(NDF,*)

      REAL(8), POINTER :: W(:,:)

      MN = MAXVAL( NPF )

      ALLOCATE( W(NDF,MN) )

      DO IP = 1, NPROCS - 1
        N = NPF(IP)
        W(:,1:N) = X(:,IPF(1:N,IP))
        CALL M_MPI_SEND_D(W,NDF*N,IP)
      ENDDO

      DEALLOCATE( W )

      END
