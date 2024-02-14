      SUBROUTINE GATHER_NODAL_D(X,NDF)

      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(NDF,*)

      REAL(8), POINTER :: W(:,:)

      MN = MAXVAL( NN_INT )

      ALLOCATE( W(NDF,MN) )

      DO IP = 1, NPROCS - 1
        N = NN_INT(IP)
        CALL M_MPI_RECV_D(W,NDF*N,IP)
        X(:,NOD(1:N,IP)) = W(:,1:N)
      ENDDO

      DEALLOCATE( W )

      END
