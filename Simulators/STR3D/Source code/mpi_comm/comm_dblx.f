      SUBROUTINE COMM_DBLX(X,NDF)

      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(NDF,*)

      REAL(8), POINTER :: WS(:,:),WR(:,:)
      INTEGER, POINTER :: IREQ1(:),IREQ2(:)

      ALLOCATE( WS(NDF,NEXPX) )
      ALLOCATE( WR(NDF,NIMPX) )
      ALLOCATE( IREQ1(NPEXPX) )
      ALLOCATE( IREQ2(NPIMPX) )

      WS(:,:) = X(:,NODEXPX(:))

      DO IP = 1, NPEXPX
        IS = IDXEXPX(1,IP)
        IE = IDXEXPX(2,IP)
        NS = NDF * ( IE - IS + 1 )
        CALL CG_MPI_ISEND_D(WS(1,IS),NS,IPEXPX(IP)-1,IREQ1(IP))
      ENDDO

      DO IP = 1, NPIMPX
        IS = IDXIMPX(1,IP)
        IE = IDXIMPX(2,IP)
        NR = NDF * ( IE - IS + 1 )
        CALL CG_MPI_IRECV_D(WR(1,IS),NR,IPIMPX(IP)-1,IREQ2(IP))
      ENDDO

      CALL C_MPI_WAITALL(NPEXPX,IREQ1)

      CALL C_MPI_WAITALL(NPIMPX,IREQ2)

      X(:,NODIMPX(:)) = WR(:,:)

      DEALLOCATE( WS )
      DEALLOCATE( WR )
      DEALLOCATE( IREQ1 )
      DEALLOCATE( IREQ2 )

      END
