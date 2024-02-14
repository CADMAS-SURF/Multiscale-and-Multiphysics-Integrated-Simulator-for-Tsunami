      SUBROUTINE GATHER_NODAL_I(K,NDF)

      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION K(NDF,*)

      INTEGER, POINTER :: KW(:,:)

      MN = MAXVAL( NN_INT )

      ALLOCATE( KW(NDF,MN) )

      DO IP = 1, NPROCS - 1
        N = NN_INT(IP)
        CALL M_MPI_RECV_I(KW,NDF*N,IP)
        K(:,NOD(1:N,IP)) = KW(:,1:N)
      ENDDO

      DEALLOCATE( KW )

      END
