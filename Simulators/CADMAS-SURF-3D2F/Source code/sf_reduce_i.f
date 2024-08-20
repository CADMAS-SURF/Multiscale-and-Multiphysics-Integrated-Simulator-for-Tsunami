      SUBROUTINE SF_REDUCE_I(K0,N0,K,IDX,N)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION K0(N0),K(N),IDX(N)

      INTEGER, POINTER :: IDXW(:),KW(:)

      IF( MYRANK == 0 ) THEN

        K0(:) = 0.

        IF( N > 0 ) K0( IDX(:) ) = K0( IDX(:) ) + K(:)

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(NW,1,IP)

          IF( NW == 0 ) CYCLE

          ALLOCATE( IDXW(NW) )
          ALLOCATE( KW(NW) )

          CALL SF_MPI_RECV_I(IDXW,NW,IP)
          CALL SF_MPI_RECV_I(KW,NW,IP)

          K0( IDXW(:) ) = K0( IDXW(:) ) + KW(:)

          DEALLOCATE( IDXW )
          DEALLOCATE( KW )

        ENDDO

      ELSEIF( MYRANK > 0 ) THEN

        CALL SF_MPI_SEND_I(N,1,0)

        IF( N > 0 ) THEN
          CALL SF_MPI_SEND_I(IDX,N,0)
          CALL SF_MPI_SEND_I(K,N,0)
        ENDIF

      ENDIF

      END

