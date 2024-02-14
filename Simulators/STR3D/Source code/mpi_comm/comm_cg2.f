      SUBROUTINE COMM_CG2(X,INDF,INDP)

      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(*),INDF(6,*),INDP(*)

      REAL(8), POINTER :: WS1(:,:),WR1(:,:),WS2(:),WR2(:)
      INTEGER, POINTER :: IREQS(:,:),IREQR(:,:)

      ALLOCATE( WS1(6,NEXP) )
      ALLOCATE( WR1(6,NIMP) )
      ALLOCATE( WS2(NEXP) )
      ALLOCATE( WR2(NIMP) )
      ALLOCATE( IREQS(NPE,2) )
      ALLOCATE( IREQR(NPE,2) )

      WS1(:,:) = 0.D0
      WS2(:) = 0.D0

      DO I = 1, NEXP
        ND = NODEXP(I)
        DO J = 1, 6
          IFR = INDF(J,ND)
          IF( IFR > 0 ) WS1(J,I) = X(IFR)
        ENDDO
        IFR = INDP(ND)
        IF( IFR > 0 ) WS2(I) = X(IFR)
      ENDDO

      DO IP = 1, NPE
        IS = IDXEXP(1,IP)
        IE = IDXEXP(2,IP)
        NS = 6 * ( IE - IS + 1 )
        CALL CG_MPI_ISEND_D(WS1(1,IS),NS,IPE(IP)-1,IREQS(IP,1))
        NS = IE - IS + 1
        CALL CG_MPI_ISEND_D(WS2(IS),NS,IPE(IP)-1,IREQS(IP,2))
      ENDDO

      DO IP = 1, NPE
        IS = IDXIMP(1,IP)
        IE = IDXIMP(2,IP)
        NR = 6 * ( IE - IS + 1 )
        CALL CG_MPI_IRECV_D(WR1(1,IS),NR,IPE(IP)-1,IREQR(IP,1))
        NR = IE - IS + 1
        CALL CG_MPI_IRECV_D(WR2(IS),NR,IPE(IP)-1,IREQR(IP,2))
      ENDDO

      CALL C_MPI_WAITALL(NPE*2,IREQS)

      CALL C_MPI_WAITALL(NPE*2,IREQR)

      DO I = 1, NIMP
        ND = NODIMP(I)
        DO J = 1, 6
          IFR = INDF(J,ND)
          IF( IFR > 0 ) X(IFR) = WR1(J,I)
        ENDDO
        IFR = INDP(ND)
        IF( IFR > 0 ) X(IFR) = WR2(I)
      ENDDO

      DEALLOCATE( WS1 )
      DEALLOCATE( WR1 )
      DEALLOCATE( WS2 )
      DEALLOCATE( WR2 )
      DEALLOCATE( IREQS )
      DEALLOCATE( IREQR )

      END
