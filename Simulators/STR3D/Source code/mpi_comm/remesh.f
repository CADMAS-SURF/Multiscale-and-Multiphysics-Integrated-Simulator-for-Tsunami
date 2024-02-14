      SUBROUTINE REMESH(DELZ,SUMZ,GRID,POS,KK,IRND,IRMSH,RMSH,IELM,IBEL
     &                 ,IMPC,IFIX,DELZ1,IFIX1)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),IRND(*),GRID(3,*),POS(3,*),NP(4),IP(5),RL(3)
     &         ,IRMSH(6,*),RMSH(4,*),DELZ(*),SUMZ(*),IELM(*),IBEL(*)
     &         ,IFIX(*),DELZ1(*),IFIX1(*)

      DATA IP / 1, 2, 3, 4, 1 /

      NNOD = KK(8)
      NELM = KK(12)
      NNODC = KK(28)

      IF( MYRANK == 0 ) THEN

        CALL C_MPI_RECV_D(DELZ,NNOD,IROOTC)
        CALL C_MPI_RECV_I(IFIX,NNOD,IROOTC)

        DO I = 1, NNOD
          IF( IRND(I) == -1 .AND. IFIX(I) > 0 ) THEN
            N = IRMSH(1,I)
            NP(1:N) = IRMSH(2:1+N,I)
            IFIX( NP(1:N) ) = 1
          ENDIF
        ENDDO

        DO I = 1, NNOD
          IF( IRND(I) == 1 .AND. IFIX(I) > 0 ) DELZ(I) = 0.D0
        ENDDO

      ELSE

        IF( MYRANK == 1 ) CALL M_MPI_SEND_I(16,1,0)  ! SEND IOP=16 TO GLB_COMM

        CALL M_MPI_RECV_I(IFIX,NNOD,0)
        CALL M_MPI_RECV_I(NDZ1,1,0)

        IFIX1(1:NDZ1) = 0

        DO I = 1, NNOD
          IF( IRND(I) == -1 .AND. IFIX(I) > 0 ) THEN
            N = IRMSH(1,I)
            NP(1:N) = IRMSH(2:1+N,I)
            IFIX1( NP(1:N) ) = 1
          ENDIF
        ENDDO

        CALL M_MPI_SEND_I(IFIX1,NDZ1,0)

        CALL M_MPI_RECV_D(DELZ,NNOD,0)
        CALL M_MPI_RECV_D(DELZ1,NDZ1,0)

      ENDIF

      DO I = 1, NNOD

        IF( IRND(I) /= -1 ) CYCLE

        N = IRMSH(1,I)
        NP(1:N) = IRMSH(2:1+N,I)
        J = IRMSH(6,I)
        RL(:) = RMSH(1:3,I)
        RZ = RMSH(4,I)

        IF( MYRANK == 0 ) THEN
          SELECT CASE( N )
          CASE( 3 )
            DZ = RL(1) * DELZ(NP(1)) + RL(2) * DELZ(NP(2))
     &         + RL(3) * DELZ(NP(3))
          CASE( 4 )
            DZ = RL(1) * DELZ(NP(IP(J))) + RL(2) * DELZ(NP(IP(J+1)))
     &         + RL(3) * .25D0 * ( DELZ(NP(1)) + DELZ(NP(2))
     &                           + DELZ(NP(3)) + DELZ(NP(4)) )
          END SELECT
        ELSE
          SELECT CASE( N )
          CASE( 3 )
            DZ = RL(1) * DELZ1(NP(1)) + RL(2) * DELZ1(NP(2))
     &         + RL(3) * DELZ1(NP(3))
          CASE( 4 )
            DZ = RL(1) * DELZ1(NP(IP(J))) + RL(2) * DELZ1(NP(IP(J+1)))
     &         + RL(3) * .25D0 * ( DELZ1(NP(1)) + DELZ1(NP(2))
     &                           + DELZ1(NP(3)) + DELZ1(NP(4)) )
          END SELECT
        ENDIF

        DELZ(I) = RZ * DZ

      ENDDO

      SUMZ(1:NNOD) = SUMZ(1:NNOD) + DELZ(1:NNOD)
      GRID(3,1:NNOD) = GRID(3,1:NNOD) + DELZ(1:NNOD)
      POS(3,1:NNOD) = POS(3,1:NNOD) + DELZ(1:NNOD)
      IF( IMPC > 0 )
     &  CALL GSURF(POS(1,NNOD+NNODC+1),POS,IELM,KK(37),NELM,IBEL,3)

      END
