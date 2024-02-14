      SUBROUTINE SF_RECV_CONT()

      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION IP(3),JP(4),X(3,3)

      INTEGER, POINTER :: ICSF(:,:)

      IF( MYRANK == 0 ) THEN
        CALL SF_C_MPI_RECV_I(ICON,1,IROOTS)
        IF( ICON > 0 ) ICON = 1
      ENDIF

      CALL VF_P1BCSI(ICON,1,0)

      IF( ICON == 0 .OR. MYRANK > 0 ) RETURN

      CALL SF_C_MPI_RECV_I(NICRG,1,IROOTS)
      CALL SF_C_MPI_RECV_I(NNCRG,1,IROOTS)

      ALLOCATE( ICRG(NICRG), STAT=IERR )
      ALLOCATE( ICSF(5,NNCRG), STAT=IERR )
      ALLOCATE( ICTB(NICRG,NICRG), STAT=IERR )
      IF( IERR /= 0 ) CALL VF_A2ERR('SF_RECV_CONT','CAN NOT ALLOC.')
      
      CALL SF_C_MPI_RECV_I(ICRG,NICRG,IROOTS)
      CALL SF_C_MPI_RECV_I(ICSF,5*NNCRG,IROOTS)
      CALL SF_C_MPI_RECV_I(ICTB,NICRG*NICRG,IROOTS)

      ! ICSF の４角形面は２つの３角形面に分割して ICTR にセット
      !      の３角形面は３角形面のまま

      NCTR = 0

      DO I = 1, NNCRG
        SELECT CASE( ICSF(1,I) )
        CASE( 3 )
          NCTR = NCTR + 1
        CASE( 4 )
          NCTR = NCTR + 2
        END SELECT
      ENDDO

      ALLOCATE( ICTR(4,NCTR), STAT=IERR )
      IF( IERR /= 0 ) CALL VF_A2ERR('SF_RECV_CONT','CAN NOT ALLOC.')

      J = 0

      DO IBDY = 1, NICRG

        IF( IBDY == 1 ) THEN
          IS = 1
        ELSE
          IS = IE + 1
        ENDIF

        IE = ICRG(IBDY)

        DO I = IS, IE
          SELECT CASE( ICSF(1,I) )
          CASE( 3 )
            J = J + 1
            ICTR(2,J) = ICSF(4,I)
            ICTR(3,J) = ICSF(3,I)
            ICTR(4,J) = ICSF(2,I)
          CASE( 4 )
            J = J + 1
            ICTR(2,J) = ICSF(2,I)
            ICTR(3,J) = ICSF(4,I)
            ICTR(4,J) = ICSF(3,I)
            J = J + 1
            ICTR(2,J) = ICSF(2,I)
            ICTR(3,J) = ICSF(5,I)
            ICTR(4,J) = ICSF(4,I)
          END SELECT
        ENDDO

        ICRG(IBDY) = J  ! ICRG セットし直し(ICSF用 -> ICTR用)

      ENDDO

      DEALLOCATE( ICSF, STAT=IERR )
      IF( IERR /= 0 ) CALL VF_A2ERR('SF_RECV_CONT','CAN NOT DEALLOC.')

      DO I = 1, NCTR
        IP(:) = ICTR(2:4,I)
        DO J = 1, NPFC0
          NP = IPFACE0(4,J)
          JP(1:NP) = IPFACE0(5:4+NP,J)
          CALL SF_COLTR(IBING,IP,JP,NP)
          IF( IBING == 1 ) THEN
            ICTR(1,I) = J
            GOTO 10
          ENDIF
        ENDDO
        CALL VF_A2ERR('SF_RECV_CONT','P.G ERROR.')
   10 ENDDO

      E_MEAN = 0.

      DO I = 1, NCTR
        X(:,:) = POS0(:,ICTR(2:4,I))
        CALL SF_STRIA(S,X,3)
        EDGE = DSQRT(2.D0*S)
        E_MEAN = E_MEAN + EDGE
      ENDDO

      E_MEAN = E_MEAN / NCTR

      END
