      SUBROUTINE SF_BCAST_I(K,IDX,N,K0,N0,M)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION K(M,N),IDX(N),K0(M,N0)

      INTEGER, POINTER :: IDXW(:),KW(:,:)

      IF( MYRANK == 0 ) THEN

        IF( N > 0 ) K(:,:) = K0(:,IDX(:))

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(NW,1,IP)

          IF( NW == 0 ) CYCLE

          ALLOCATE( IDXW(NW) )
          ALLOCATE( KW(M,NW) )

          CALL SF_MPI_RECV_I(IDXW,NW,IP)

          KW(:,:) = K0(:,IDXW(:))

          CALL SF_MPI_SEND_I(KW,M*NW,IP)

          DEALLOCATE( IDXW )
          DEALLOCATE( KW )

        ENDDO

      ELSEIF( MYRANK > 0 ) THEN

        CALL SF_MPI_SEND_I(N,1,0)

        IF( N > 0 ) THEN

          CALL SF_MPI_SEND_I(IDX,N,0)

          CALL SF_MPI_RECV_I(K,M*N,0)

        ENDIF

      ENDIF

      END

