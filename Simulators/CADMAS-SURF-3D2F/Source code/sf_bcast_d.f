      SUBROUTINE SF_BCAST_D(X,IDX,N,X0,N0,M)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION X(M,N),IDX(N),X0(M,N0)

      INTEGER, POINTER :: IDXW(:)
      REAL(8), POINTER :: XW(:,:)

      IF( MYRANK == 0 ) THEN

        IF( N > 0 ) X(:,:) = X0(:,IDX(:))

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(NW,1,IP)

          IF( NW == 0 ) CYCLE

          ALLOCATE( IDXW(NW) )
          ALLOCATE( XW(M,NW) )

          CALL SF_MPI_RECV_I(IDXW,NW,IP)

          XW(:,:) = X0(:,IDXW(:))

          CALL SF_MPI_SEND_D(XW,M*NW,IP)

          DEALLOCATE( IDXW )
          DEALLOCATE( XW )

        ENDDO

      ELSEIF( MYRANK > 0 ) THEN

        CALL SF_MPI_SEND_I(N,1,0)

        IF( N > 0 ) THEN

          CALL SF_MPI_SEND_I(IDX,N,0)

          CALL SF_MPI_RECV_D(X,M*N,0)

        ENDIF

      ENDIF

      END

