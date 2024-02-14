      SUBROUTINE PART_C(KK,ITO)

      USE M_VAL
      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      INTEGER*8 MUSED

      DIMENSION KK(*)

      INTEGER, POINTER :: INP(:),IFIL(:),IECW(:,:),IEGW(:,:),IEQW(:,:)
      INTEGER, POINTER :: NODCW(:,:),IELMCW(:,:),IBEC(:),IFEL(:),IFND(:)
      INTEGER, POINTER :: IBTA(:,:),IBTW(:,:),INCW(:,:)

      WRITE(ITO,'(/X,A,F8.1,A)')
     &  'SUB. PART_C     : ', MUSED()*4.D-6, '(MB) USED.'

      NNOD = KK(8)
      NSOL  = KK(10)
      NELM  = KK(12)
      NIGSF = KK(94)
      NIELC = KK(97)
      NIEDG = KK(99)
      NIBTE = KK(100)
      NIELQ = KK(101)
      NINDC0 = KK(102)

      NPECG = NPROCS - 1

!     --- INP ---

      ALLOCATE( INP(NNOD+NIGSF) )

      INP(:) = 0

      DO IP = 1, NPECG
        INP( NOD(1:NN_INT(IP),IP) ) = IP
      ENDDO

      DO I = NNOD + 1, NNOD + NIGSF
        J = IVRQ(I)
        IF( J > 0 ) 
     &    INP(I) = MIN0( INP( IELQ(1,J) ), INP( IELQ(2,J) ),
     &                   INP( IELQ(3,J) ), INP( IELQ(4,J) ) )
      ENDDO

!     --- NG, NODG ---

      MG = 0

      DO IP = 1, NPECG

        IG = 0

        DO I = 1, NE(IP)

          IE = IEL(I,IP)

          IF( IBEL(IE) == 0 ) CYCLE

          N = IELM(3,IE)

          SELECT CASE( N )
          CASE( 6 )
            IG = IG + 3
          CASE( 8 )
            IG = IG + 6
          END SELECT

        ENDDO

        MG = MAX0(MG,IG)

      ENDDO

      ALLOCATE( NG(NPECG) )
      ALLOCATE( NODG(MG,NPECG) )

      DO IP = 1, NPECG

        IG = 0

        DO I = 1, NE(IP)

          IE = IEL(I,IP)

          IF( IBEL(IE) == 0 ) CYCLE

          N = IELM(3,IE)

          SELECT CASE( N )
          CASE( 6 )
            NODG(IG+1:IG+3,IP) = IGSFR(3:5,IE)
            IG = IG + 3
          CASE( 8 )
            NODG(IG+1:IG+6,IP) = IGSFR(1:6,IE)
            IG = IG + 6
          END SELECT

        ENDDO

        NG(IP) = IG

      ENDDO

!     --- NODP ---

      ALLOCATE( NODP(2,NNOD+NIGSF) )

      NODP(:,:) = 0

      DO IP = 1, NPECG

        DO I = 1, NN_INT(IP)
          J = NOD(I,IP)
          NODP(1,J) = IP
          NODP(2,J) = I
        ENDDO

        DO I = 1, NG(IP)
          J = NODG(I,IP)
          IF( INP(J) == IP ) THEN
            NODP(1,J) = IP
            NODP(2,J) = I + NN_EXT(IP)
          ENDIF
        ENDDO

      ENDDO

!     --- IEC, NIEC, IELCP ---

      ALLOCATE( IECW(NIELC,NPECG) )
      ALLOCATE( NIEC(NPECG) )
      ALLOCATE( IELCP(2,NIELC) )

      ALLOCATE( IFIL(NNOD+NIGSF) )

      IELCP(:,:) = 0

      DO IP = 1, NPECG

        IFIL(:) = 0

        IFIL( NOD(1:NN_EXT(IP),IP) ) = 1

        IFIL( NODG(1:NG(IP),IP) ) = 1

        IC = 0

        DO I = 1, NIELC

          DO J = 1, 3
            IF( IFIL( IELC(J,I) ) == 0 ) GOTO 10
          ENDDO

          IC = IC + 1
          IECW(IC,IP) = I

   10   ENDDO

        NIEC(IP) = IC

        DO I = 1, NIEC(IP)

          IE = IECW(I,IP)

          IF( IELCP(1,IE) /= 0 ) CYCLE

          DO J = 1, 3
            IF( INP( IELC(J,IE) ) == IP ) THEN
              IELCP(1,IE) = IP
              IELCP(2,IE) = I
              EXIT
            ENDIF
          ENDDO

        ENDDO

      ENDDO

      ALLOCATE( IEC(MAXVAL(NIEC),NPECG) )

      DO IP = 1, NPECG
        IEC(1:NIEC(IP),IP) = IECW(1:NIEC(IP),IP)
      ENDDO

      DEALLOCATE( IECW )

!     --- INC, NINC, INDCP ---

      ALLOCATE( INCW(NINDC0,NPECG) )
      ALLOCATE( NINC(NPECG) )
      ALLOCATE( INDCP(2,NINDC0) )

      DO IP = 1, NPECG

        IFIL(:) = 0

        IFIL( NOD(1:NN_EXT(IP),IP) ) = 1

        IC = 0

        DO I = 1, NINDC0

          IF( IFIL( INDC0(I) ) == 1 ) THEN
            IC = IC + 1
            INCW(IC,IP) = I
          ENDIF

        ENDDO

        NINC(IP) = IC

        DO I = 1, NINC(IP)
          J = INCW(I,IP)
          IF( INP( INDC0(J) ) == IP ) THEN
            INDCP(1,J) = IP
            INDCP(2,J) = I
          ENDIF
        ENDDO

      ENDDO

      ALLOCATE( INC(MAXVAL(NINC),NPECG) )

      DO IP = 1, NPECG
        INC(1:NINC(IP),IP) = INCW(1:NINC(IP),IP)
      ENDDO

      DEALLOCATE( INCW )

!     --- IEG, NIEG, IEDGP ---

      ALLOCATE( IEGW(NIEDG,NPECG) )
      ALLOCATE( NIEG(NPECG) )
      ALLOCATE( IEDGP(2,NIEDG) )

      IEDGP(:,:) = 0

      DO IP = 1, NPECG

        IFIL(:) = 0

        IFIL( NOD(1:NN_EXT(IP),IP) ) = 1

        IFIL( NODG(1:NG(IP),IP) ) = 1

        IC = 0

        DO I = 1, NIEDG

          IF( IFIL( IEDG(1,I) ) == 1 .AND. IFIL( IEDG(2,I) ) == 1 ) THEN
            IC = IC + 1
            IEGW(IC,IP) = I
          ENDIF

        ENDDO

        NIEG(IP) = IC

        DO I = 1, NIEG(IP)

          IE = IEGW(I,IP)

          IF( IEDGP(1,IE) /= 0 ) CYCLE

          IF( INP( IEDG(1,IE) ) == IP .OR. INP( IEDG(2,IE) ) == IP )THEN
            IEDGP(1,IE) = IP
            IEDGP(2,IE) = I
          ENDIF

        ENDDO

      ENDDO

      ALLOCATE( IEG(MAXVAL(NIEG),NPECG) )

      DO IP = 1, NPECG
        IEG(1:NIEG(IP),IP) = IEGW(1:NIEG(IP),IP)
      ENDDO

      DEALLOCATE( IEGW )

      DEALLOCATE( INP )

!     --- IEQ, NIEQ ---

      ALLOCATE( IEQW(NIELQ,NPECG) )
      ALLOCATE( NIEQ(NPECG) )

      DO IP = 1, NPECG

        IFIL(:) = 0

        IFIL( NOD(1:NN_EXT(IP),IP) ) = 1

        IC = 0

        DO I = 1, NIELQ

          DO J = 1, 4
            IF( IFIL( IELQ(J,I) ) == 0 ) GOTO 20
          ENDDO

          IC = IC + 1
          IEQW(IC,IP) = I

   20   ENDDO

        NIEQ(IP) = IC

      ENDDO

      ALLOCATE( IEQ(MAXVAL(NIEQ),NPECG) )

      DO IP = 1, NPECG
        IEQ(1:NIEQ(IP),IP) = IEQW(1:NIEQ(IP),IP)
      ENDDO

      DEALLOCATE( IEQW )

      DEALLOCATE( IFIL )

!     --- NN_EXTC, NEC, NODC, IELMC ---

      ALLOCATE( NN_EXTC(NPECG) )
      ALLOCATE( NEC(NPECG) )

      ALLOCATE( NODCW( MAXVAL(NN_INT), NPECG ) )
      ALLOCATE( IELMCW( MAXVAL(NE), NPECG ) )

      ALLOCATE( IBEC(NNOD+NIGSF) )
      ALLOCATE( IFEL(NELM) )
      ALLOCATE( IFND(NNOD) )

      DO IP = 1, NPECG

        IBEC(:) = 0

        DO I = 1, NIEC(IP)
          IC = IEC(I,IP)
          IF( IELCP(1,IC) == IP ) IBEC( IELC(:,IC) ) = 1
        ENDDO

        IFEL(:) = 0

        IFEL( IEL(1:NE(IP),IP) ) = 1

        IFND(:) = 0

        IFND( NOD(1:NN_EXT(IP),IP) ) = 1

        IE = 0
        IN = 0

        DO I = 1, NSOL
          IF( IFEL(I) == 1 ) CYCLE
          N = IELM(3,I)
          DO J = 1, N
            JN = IELM(7+J,I)
            IF( IBEC(JN) == 1 ) THEN
              IE = IE + 1
              IELMCW(IE,IP) = I
              DO K = 1, N
                KN = IELM(7+K,I)
                IF( IFND(KN) == 0 ) THEN
                  IN = IN + 1
                  NODCW(IN,IP) = KN
                  IFND(KN) = 1
                ENDIF
              ENDDO
              EXIT
            ENDIF
          ENDDO
        ENDDO

        NEC(IP) = IE
        NN_EXTC(IP) = IN

      ENDDO

      ALLOCATE( NODC( MAXVAL(NN_EXTC), NPECG ) )
      ALLOCATE( IELMC( MAXVAL(NEC), NPECG ) )

      DO IP = 1, NPECG
        NODC(1:NN_EXTC(IP),IP) = NODCW(1:NN_EXTC(IP),IP)
        IELMC(1:NEC(IP),IP) = IELMCW(1:NEC(IP),IP)
      ENDDO

      DEALLOCATE( NODCW )
      DEALLOCATE( IELMCW )

      DEALLOCATE( IBEC )
      DEALLOCATE( IFEL )
      DEALLOCATE( IFND )

!     --- NGC, NODGC ---

      MG = 0

      DO IP = 1, NPECG

        IG = 0

        DO I = 1, NEC(IP)

          IE = IELMC(I,IP)

          N = IELM(3,IE)

          SELECT CASE( N )
          CASE( 6 )
            IG = IG + 3
          CASE( 8 )
            IG = IG + 6
          END SELECT

        ENDDO

        MG = MAX0(MG,IG)

      ENDDO

      ALLOCATE( NGC(NPECG) )
      ALLOCATE( NODGC(MG,NPECG) )

      DO IP = 1, NPECG

        IG = 0

        DO I = 1, NEC(IP)

          IE = IELMC(I,IP)

          N = IELM(3,IE)

          SELECT CASE( N )
          CASE( 6 )
            NODGC(IG+1:IG+3,IP) = IGSFR(3:5,IE)
            IG = IG + 3
          CASE( 8 )
            NODGC(IG+1:IG+6,IP) = IGSFR(1:6,IE)
            IG = IG + 6
          END SELECT

        ENDDO

        NGC(IP) = IG

      ENDDO

      DEALLOCATE( IGSFR )

!     --- NODP 修正 ---

      DO I = NNOD + 1, NNOD + NIGSF
        IP = NODP(1,I)
        IF( IP > 0 ) NODP(2,I) = NODP(2,I) + NN_EXTC(IP)
      ENDDO

!     --- IBT, NIBT ---

      ALLOCATE( IBTA(2,NSOL) )

      IBTA(:,:) = 0

      IC = 1

      DO I = 1, NSOL

        IF( IBEL(I) == 0 ) CYCLE

        N = IELM(3,I)

        SELECT CASE( N )
        CASE( 4 )
          IBTA(1,I) = IC
          IBTA(2,I) = IC
          IC = IC + 1
        CASE( 6 )
          IBTA(1,I) = IC
          IBTA(2,I) = IC + 10
          IC = IC + 11
        CASE( 8 )
          IBTA(1,I) = IC
          IBTA(2,I) = IC + 23
          IC = IC + 24
        END SELECT

      ENDDO

      ALLOCATE( NIBT(NPECG) )
      ALLOCATE( IBTW(NIBTE,NPECG) )

      DO IP = 1, NPECG

        IC = 0

        DO I = 1, NE(IP)

          IE = IEL(I,IP)

          IF( IBEL(IE) == 0 ) CYCLE

          JS = IBTA(1,IE)
          JE = IBTA(2,IE)

          DO J = JS, JE
            IC = IC + 1
            IBTW(IC,IP) = J
          ENDDO

        ENDDO

        DO I = 1, NEC(IP)

          IE = IELMC(I,IP)

          IF( IBEL(IE) == 0 ) CYCLE

          JS = IBTA(1,IE)
          JE = IBTA(2,IE)

          DO J = JS, JE
            IC = IC + 1
            IBTW(IC,IP) = J
          ENDDO

        ENDDO

        NIBT(IP) = IC

      ENDDO

      ALLOCATE( IBT(MAXVAL(NIBT),NPECG) )

      DO IP = 1, NPECG
        IBT(1:NIBT(IP),IP) = IBTW(1:NIBT(IP),IP)
      ENDDO

      DEALLOCATE( IBTW )

      DEALLOCATE( IBTA )

      END
