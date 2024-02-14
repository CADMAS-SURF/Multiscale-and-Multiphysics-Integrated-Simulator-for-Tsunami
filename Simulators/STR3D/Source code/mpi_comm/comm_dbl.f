      SUBROUTINE COMM_DBL(X,NDF)

      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(NDF,*)

      REAL(8), POINTER :: WS(:,:),WR(:,:)
      INTEGER, POINTER :: IREQ1(:),IREQ2(:)

      ALLOCATE( WS(NDF,NEXP) )
      ALLOCATE( WR(NDF,NIMP) )
      ALLOCATE( IREQ1(NPE) )
      ALLOCATE( IREQ2(NPE) )

      WS(:,:) = X(:,NODEXP(:))

      DO IP = 1, NPE
        IS = IDXEXP(1,IP)
        IE = IDXEXP(2,IP)
        NS = NDF * ( IE - IS + 1 )
        CALL CG_MPI_ISEND_D(WS(1,IS),NS,IPE(IP)-1,IREQ1(IP))
      ENDDO

      DO IP = 1, NPE
        IS = IDXIMP(1,IP)
        IE = IDXIMP(2,IP)
        NR = NDF * ( IE - IS + 1 )
        CALL CG_MPI_IRECV_D(WR(1,IS),NR,IPE(IP)-1,IREQ2(IP))
      ENDDO

      CALL C_MPI_WAITALL(NPE,IREQ1)

      CALL C_MPI_WAITALL(NPE,IREQ2)

      X(:,NODIMP(:)) = WR(:,:)

      DEALLOCATE( WS )
      DEALLOCATE( WR )
      DEALLOCATE( IREQ1 )
      DEALLOCATE( IREQ2 )

      END
