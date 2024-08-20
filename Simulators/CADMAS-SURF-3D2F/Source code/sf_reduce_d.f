      SUBROUTINE SF_REDUCE_D(X0,N0,X,IDX,N)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION X0(N0),X(N),IDX(N)

      INTEGER, POINTER :: IDXW(:)
      REAL(8), POINTER :: XW(:)

      IF( MYRANK == 0 ) THEN

        X0(:) = 0.

        IF( N > 0 ) X0( IDX(:) ) = X0( IDX(:) ) + X(:)

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(NW,1,IP)

          IF( NW == 0 ) CYCLE

          ALLOCATE( IDXW(NW) )
          ALLOCATE( XW(NW) )

          CALL SF_MPI_RECV_I(IDXW,NW,IP)
          CALL SF_MPI_RECV_D(XW,NW,IP)

          X0( IDXW(:) ) = X0( IDXW(:) ) + XW(:)

          DEALLOCATE( IDXW )
          DEALLOCATE( XW )

        ENDDO

      ELSEIF( MYRANK > 0 ) THEN

        CALL SF_MPI_SEND_I(N,1,0)

        IF( N > 0 ) THEN
          CALL SF_MPI_SEND_I(IDX,N,0)
          CALL SF_MPI_SEND_D(X,N,0)
        ENDIF

      ENDIF

      END

