      SUBROUTINE ELEM_INIT(KK)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),D(21)

      NELM = KK(12)
      MGP = KK(36)

      DO I = 1, NELM
        ITYP = IELM(2,I)
        IMAT = IELM(4,I)
        SELECT CASE( ITYP )
        CASE( 2, 6 )
          CALL DSOL0(D,AMAT(1,IMAT),AMAT(2,IMAT))
          N = 21
        CASE( 3 )
          D(1) = AMAT(1,IMAT)
          N = 1
        CASE( 4 )
          CALL DBEAM0(D,AMAT(1,IMAT),AMAT(2,IMAT))
          N = 6
        END SELECT
        IE = 0
        DO J = 1, MGP
          DMT(IE+1:IE+N,I) = D(1:N)
          IE = IE + N
        ENDDO
      ENDDO

      DO I = 1, NELM
        IMAT = IELM(4,I)
        IYLD = MAT(2,IMAT)
        ST = AMAT(14,IMAT)
        IF( IYLD > 0 .OR. ST > 0. ) THEN
          KK(80) = 1
          EXIT
        ENDIF
      ENDDO

      IF( MYRANK > 0 ) THEN
        CALL CG_MPI_ALLREDUCE_I(KK(80),MAX,1,1)
        KK(80) = MAX
        IF( MYRANK == 1 ) CALL SEND_KK(KK,80)
      ENDIF

      IF( KK(80) == 1 ) THEN

        ALLOCATE( IST(MGP,NELM) )
        ALLOCATE( SIGY(MGP,NELM) )

        IST(:,:) = 0

        DO I = 1, NELM
          ITYP = IELM(2,I)
          IMAT = IELM(4,I)
          SIGY(:,I) = AMAT(11,IMAT)
          IF( ITYP == 3 ) SIGY(2,I) = SIGY(2,I) / AMAT(1,IMAT)
        ENDDO

      ENDIF

      IF( KK(16) > 0 ) THEN

        DO I = 1, NELM
          ITYP = IELM(2,I)
          IF( ITYP == 4 ) THEN
            ID = IELM(7,I)
            CALL DIRINIT(VG0(1,1,1,ID),IELM(8,I),BVEC(1,ID),GRID)
          ENDIF
        ENDDO

        VG = VG0

      ENDIF

      END