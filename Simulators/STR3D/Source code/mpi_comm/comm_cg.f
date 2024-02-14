      SUBROUTINE COMM_CG(X,INDF,NDF)

      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(*),INDF(NDF,*)

      REAL(8), POINTER :: WS(:,:),WR(:,:)
      INTEGER, POINTER :: IREQ1(:),IREQ2(:)

      ALLOCATE( WS(NDF,NEXP) )
      ALLOCATE( WR(NDF,NIMP) )
      ALLOCATE( IREQ1(NPE) )
      ALLOCATE( IREQ2(NPE) )

      WS(:,:) = 0.D0

      DO I = 1, NEXP
        ND = NODEXP(I)
        DO J = 1, NDF
          IFR = INDF(J,ND)
          IF( IFR > 0 ) WS(J,I) = X(IFR)
        ENDDO
      ENDDO

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

      DO I = 1, NIMP
        ND = NODIMP(I)
        DO J = 1, NDF
          IFR = INDF(J,ND)
          IF( IFR > 0 ) X(IFR) = WR(J,I)
        ENDDO
      ENDDO

      DEALLOCATE( WS )
      DEALLOCATE( WR )
      DEALLOCATE( IREQ1 )
      DEALLOCATE( IREQ2 )

      END
