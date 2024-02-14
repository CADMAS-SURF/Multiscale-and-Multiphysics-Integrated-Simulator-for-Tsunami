      SUBROUTINE CONT_TBL(KK,ITO)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),NOD(2,3),NP(8),XX(3,3),GX(3),IG(2)
      INTEGER, POINTER :: IFIL(:),NED(:),IED(:,:,:),IEP(:,:),IEDGW(:,:)
     &                   ,IEDQW(:),NCEL(:),NCFC(:),NCED(:)
      DATA NOD / 2,3, 3,1, 1,2 /

      NNOD  = KK( 8)
      NSOL  = KK(10)
      NELM  = KK(12)
      NICRG = KK(92)
      NNCRG = KK(93)

!     --- ICSF ---

      ALLOCATE( ICSF(6,NNCRG) )

      ICSF(:,:) = 0

      NIELQ = 0

      DO I = 1, NNCRG

        IEL = NCRG(1,I)
        NNP = IELM(3,IEL)

        SELECT CASE( NNP )
        CASE( 4 )
          CALL CSFTE(ICSF(1,I),NCRG(2,I),IELM(8,IEL),ITO)
        CASE( 6 )
          CALL CSFPN(ICSF(1,I),NCRG(2,I),IELM(8,IEL),ITO)
        CASE( 8 )
          CALL CSFHX(ICSF(1,I),NCRG(2,I),IELM(8,IEL),ITO)
        END SELECT

        IF( ICSF(2,I) == 4 ) NIELQ = NIELQ + 1

      ENDDO

!     --- IELQ ---

      ALLOCATE( IELQ(4,NIELQ) )

      IP = 0

      DO I = 1, NNCRG
        IF( ICSF(2,I) == 4 ) THEN
          IP = IP + 1
          IELQ(:,IP) = ICSF(3:6,I)
        ENDIF
      ENDDO

!     --- IBEL, IGSFR, IBTE ---

      ALLOCATE( IFIL(NNOD) )

      IFIL(:) = 0

      DO I = 1, NNCRG
        N = ICSF(2,I)
        IFIL( ICSF(3:2+N,I) ) = 1
      ENDDO

      ALLOCATE( IBEL(NELM) )

      IBEL(:) = 0

      NIBTE = 0

      DO I = 1, NSOL

        NNP = IELM(3,I)
        NP(1:NNP) = IELM(8:7+NNP,I)

        DO J = 1, NNP

          IF( IFIL( NP(J) ) == 1 ) THEN

            IBEL(I) = 1

            SELECT CASE( NNP )
            CASE( 4 )
              NT = 1
            CASE( 6 )
              NT = 11
            CASE( 8 )
              NT = 24
            END SELECT

            NIBTE = NIBTE + NT

            EXIT

          ENDIF

        ENDDO

      ENDDO

      DEALLOCATE( IFIL )

      ALLOCATE( IGSFR(6,NSOL) )
      ALLOCATE( IBTE(4,NIBTE) )

      IGSFR(:,:) = 0

      JP = 0
      IP = 1

      DO I = 1, NSOL

        IF( IBEL(I) == 0 ) CYCLE

        NNP = IELM(3,I)
        NP(1:NNP) = IELM(8:7+NNP,I)

        SELECT CASE( NNP )
        CASE( 4 )
          IBTE(:,IP) = NP(1:4)
          IP = IP + 1
        CASE( 6 )
          DO M = 3, 5
            JP = JP + 1
            IGSFR(M,I) = JP + NNOD
          ENDDO
          CALL MKTEPN(IBTE(1,IP),NP,IGSFR(3,I))
          IP = IP + 11
        CASE( 8 )
          DO M = 1, 6
            JP = JP + 1
            IGSFR(M,I) = JP + NNOD
          ENDDO
          CALL MKTEHX(IBTE(1,IP),NP,IGSFR(1,I))
          IP = IP + 24
        END SELECT

      ENDDO

      NIGSF = JP
      KK(94) = NIGSF

!     --- ICBD, IELC, IFCQ, IVRQ ---

      NIELC = 0

      DO I = 1, NNCRG
        SELECT CASE( ICSF(2,I) )
        CASE( 3 )
          NIELC = NIELC + 1
        CASE( 4 )
          NIELC = NIELC + 4
        END SELECT
      ENDDO

      KK(97) = NIELC

      ALLOCATE( ICBD(NICRG) )
      ALLOCATE( IELC(3,NIELC) )
      ALLOCATE( IFCQ(NIELC) )
      ALLOCATE( IVRQ(NNOD+NIGSF) )

      IFCQ(:) = 0
      IVRQ(:) = 0

      IP = 1
      JP = 1

      DO IRG = 1, NICRG

        CALL ADDSET3(IS,IE,ICRG,IRG)

        DO I = IS, IE

          SELECT CASE( ICSF(2,I) )
          CASE( 3 )

            IELC(:,IP) = ICSF(3:5,I)

            IP = IP + 1

          CASE( 4 )

            IG5 = IGSFR(ICSF(1,I),NCRG(1,I))

            CALL ELCQUAD(IELC(1,IP),ICSF(1,I),IG5)

            IFCQ(IP:IP+3) = JP

            IVRQ(IG5) = JP

            IP = IP + 4

            JP = JP + 1

          END SELECT

        ENDDO

        ICBD(IRG) = IP - 1

      ENDDO

      DEALLOCATE( IGSFR )

!     --- INDA, INDC ---

      ALLOCATE( INDA(NICRG) )

      ALLOCATE( IFIL(NNOD) )

      IFIL(:) = 0

      DO I = 1, NNCRG
        N = ICSF(2,I)
        IFIL( ICSF(3:2+N,I) ) = 1
      ENDDO

      NINDC = SUM(IFIL)

      KK(95) = NINDC

      ALLOCATE( INDC(NINDC) )

      IFIL(:) = 0

      IP = 0

      DO IRG = 1, NICRG

        CALL ADDSET3(IS,IE,ICRG,IRG)

        DO I = IS, IE
          N = ICSF(2,I)
          DO J = 1, N
            JG = ICSF(2+J,I)
            IF( IFIL(JG) == 1 ) CYCLE
            IP = IP + 1
            INDC(IP) = JG
            IFIL(JG) = 1
          ENDDO
        ENDDO

        INDA(IRG) = IP

      ENDDO

      DEALLOCATE( IFIL )

!     --- IEDA, IEDG, IEDQ ---

      ALLOCATE( NED(NNOD) )

      NED(:) = 0

      DO I = 1, NIELC
        IF( IFCQ(I) == 0 ) THEN
          KS = 1
        ELSE
          KS = 3
        ENDIF
        DO K = KS, 3
          IG1 = IELC(NOD(1,K),I)
          NED(IG1) = NED(IG1) + 1
        ENDDO
      ENDDO

      ALLOCATE( IED(2,MAXVAL(NED),NNOD) )

      NED(:) = 0

      DO I = 1, NIELC
        IF( IFCQ(I) == 0 ) THEN
          KS = 1
        ELSE
          KS = 3
        ENDIF
        DO K = KS, 3
          IG1 = IELC(NOD(1,K),I)
          NED(IG1) = NED(IG1) + 1
          IED(1,NED(IG1),IG1) = I
          IED(2,NED(IG1),IG1) = K
        ENDDO
      ENDDO

      ALLOCATE( IEDA(NICRG) )

      ALLOCATE( IEP(3,NIELC) )

      ALLOCATE( IEDGW(6,3*NIELC) )
      ALLOCATE( IEDQW(3*NIELC) )

      IEP(:,:) = 0

      IEDGW(:,:) = 0
      IEDQW(:) = 0

      IP = 1
      JP = 1
      NIEDG = 0

      DO IRG = 1, NICRG

        KP = NIEDG + 1

        CALL ADDSET3(IS,IE,ICRG,IRG)

        DO I = IS, IE

          SELECT CASE( ICSF(2,I) )
          CASE( 3 )

            IP = IP + 1

          CASE( 4 )

            CALL EDGQUAD(IEDGW(1,KP),IELC(1,IP),IP)

            IEDQW(KP:KP+3) = JP

            IP = IP + 4
            JP = JP + 1
            KP = KP + 4

          END SELECT

        ENDDO

        NIEDG = KP - 1

        CALL ADDSET4(IS,IE,ICBD,IRG)

        DO I = IS, IE

          IF( IFCQ(I) == 0 ) THEN
            KS = 1
          ELSE
            KS = 3
          ENDIF

          DO K = KS, 3

            IF( IEP(K,I) == 1 ) CYCLE

            IG(:) = IELC(NOD(:,K),I)

            NIEDG = NIEDG + 1

            IEDGW(1:2,NIEDG) = IG(:)
            IEDGW(3,NIEDG) = I
            IEDGW(4,NIEDG) = K

            IEP(K,I) = 1

            DO L = 1, NED(IG(2))
              I2 = IED(1,L,IG(2))
              K2 = IED(2,L,IG(2))
              IF( IEP(K2,I2) == 1 ) CYCLE
              IF( IG(1) == IELC(NOD(2,K2),I2) ) THEN
                IEDGW(5,NIEDG) = I2
                IEDGW(6,NIEDG) = K2
                IEP(K2,I2) = 1
                EXIT
              ENDIF
            ENDDO

          ENDDO

        ENDDO

        IEDA(IRG) = NIEDG

      ENDDO

      ALLOCATE( IEDG(6,NIEDG) )
      ALLOCATE( IEDQ(NIEDG) )

      IEDG(:,:)=IEDGW(:,1:NIEDG)
      IEDQ(:)=IEDQW(1:NIEDG)

      DEALLOCATE( NED )
      DEALLOCATE( IED )
      DEALLOCATE( IEP )

      DEALLOCATE( IEDGW )
      DEALLOCATE( IEDQW )

!     --- IELA ---

      ALLOCATE( IELA(3,NIELC) )

      IELA(:,:) = 0

      DO I = 1, NIEDG

        IENO1 = IEDG(3,I)
        K1 = IEDG(4,I)
        IENO2 = IEDG(5,I)
        K2 = IEDG(6,I)

        IELA(K1,IENO1) = I
        IF( IENO2 > 0 ) IELA(K2,IENO2) = I

      ENDDO

!     --- ICELA, ICEL ---

      ALLOCATE( IFIL(NNOD+NIGSF) )
      ALLOCATE( NCEL(NNOD+NIGSF) )

      IFIL(:) = 0

      IFIL( INDC(:) ) = 1

      DO I = NNOD + 1, NNOD + NIGSF
        IF( IVRQ(I) > 0 ) IFIL(I) = 1
      ENDDO

      NCEL(:) = 0

      DO I = 1, NIBTE
        DO J = 1, 4
          NODE = IBTE(J,I)
          IF( IFIL(NODE) == 1 ) NCEL(NODE) = NCEL(NODE) + 1
        ENDDO
      ENDDO

      NICEL = SUM(NCEL)

      ALLOCATE( ICELA(2,NNOD+NIGSF) )
      ALLOCATE( ICEL(NICEL) )

      ICELA(:,:) = 0

      IE = 0

      DO I = 1, NNOD + NIGSF
        N = NCEL(I)
        IF( N == 0 ) CYCLE
        IS = IE + 1
        IE = IE + N
        ICELA(1,I) = IS
        ICELA(2,I) = IE
      ENDDO

      NCEL(:) = 0

      DO I = 1, NIBTE
        DO J = 1, 4
          NODE = IBTE(J,I)
          IF( IFIL(NODE) == 1 ) THEN
            NCEL(NODE) = NCEL(NODE) + 1
            IS = ICELA(1,NODE)
            IP = IS + NCEL(NODE) - 1
            ICEL(IP) = I
          ENDIF
        ENDDO
      ENDDO

      DEALLOCATE( IFIL )
      DEALLOCATE( NCEL )

!     --- ICFCA, ICFC ---

      ALLOCATE( NCFC(NNOD+NIGSF) )

      NCFC(:) = 0

      DO I = 1, NIELC
        DO J = 1, 3
          NODE = IELC(J,I)
          NCFC(NODE) = NCFC(NODE) + 1
        ENDDO
      ENDDO

      NICFC = SUM(NCFC)

      ALLOCATE( ICFCA(2,NNOD+NIGSF) )
      ALLOCATE( ICFC(NICFC) )

      ICFCA(:,:) = 0

      IE = 0

      DO I = 1, NNOD + NIGSF
        N = NCFC(I)
        IF( N == 0 ) CYCLE
        IS = IE + 1
        IE = IE + N
        ICFCA(1,I) = IS
        ICFCA(2,I) = IE
      ENDDO

      NCFC(:) = 0

      DO I = 1, NIELC
        DO J = 1, 3
          NODE = IELC(J,I)
          NCFC(NODE) = NCFC(NODE) + 1
          IS = ICFCA(1,NODE)
          IP = IS + NCFC(NODE) - 1
          ICFC(IP) = I
        ENDDO
      ENDDO

      DEALLOCATE( NCFC )

!     --- ICEDA, ICED ---

      ALLOCATE( NCED(NNOD+NIGSF) )

      NCED(:) = 0

      DO I = 1, NIEDG
        DO J = 1, 2
          NODE = IEDG(J,I)
          NCED(NODE) = NCED(NODE) + 1
        ENDDO
      ENDDO

      NICED = SUM(NCED)

      ALLOCATE( ICEDA(2,NNOD+NIGSF) )
      ALLOCATE( ICED(NICED) )

      ICEDA(:,:) = 0

      IE = 0

      DO I = 1, NNOD + NIGSF
        N = NCED(I)
        IF( N == 0 ) CYCLE
        IS = IE + 1
        IE = IE + N
        ICEDA(1,I) = IS
        ICEDA(2,I) = IE
      ENDDO

      NCED(:) = 0

      DO I = 1, NIEDG
        DO J = 1, 2
          NODE = IEDG(J,I)
          NCED(NODE) = NCED(NODE) + 1
          IS = ICEDA(1,NODE)
          IP = IS + NCED(NODE) - 1
          ICED(IP) = I
        ENDDO
      ENDDO

      DEALLOCATE( NCED )

!     --- GELC ---

      ALLOCATE( GELC(NIELC) )

      DO I = 1, NIELC

        XX(:,1:2) = GRID(:,IELC(1:2,I))

        IG3 = IELC(3,I)
        IQU = IVRQ(IG3)
        IF( IQU == 0 ) THEN
          XX(:,3) = GRID(:,IG3)
        ELSE
          CALL MEAN4(XX(1,3),GRID,3,IELQ(1,IQU),4)
        ENDIF

        GX(:) = ( XX(:,1) + XX(:,2) + XX(:,3) ) / 3.D0

        DIST_MAX = 0.
        DO J = 1, 3
          CALL LENGTH(DIST,XX(1,J),GX,3)
          DIST_MAX = DMAX1( DIST_MAX, DIST )
        ENDDO

        GELC(I) = DIST_MAX

      ENDDO

!     --- IELG ---

      ALLOCATE( IELG(NIELC) )

      IELG(:) = 0

      IP = 0

      IE = 0

      DO I = 1, NNCRG

        IEL = NCRG(1,I)
        ITYP = IELM(2,IEL)

        IS = IE + 1

        SELECT CASE( ICSF(2,I) )
        CASE( 3 )
          IE = IE + 1
        CASE( 4 )
          IE = IE + 4
        END SELECT

        IF( ITYP == 6 ) THEN
          DO J = IS, IE
            IP = IP + 1
            IELG(IP) = J
          ENDDO
        ENDIF

      ENDDO

      KK(98) = IP

!     --- ICTB ---

      ALLOCATE( ICTB(NICRG,NICRG) )

!     --- ISLV, ISLVO, RSLV, IRANK ---

      ALLOCATE( ISLV(2,NINDC) )
      ALLOCATE( ISLVO(2,NINDC) )
      ALLOCATE( RSLV(3,NINDC) )
      ALLOCATE( IRANK(NINDC) )

!     --- FRTB, U0, RL0, IFRIC, ISTK, FRIC, EDML ---

      ALLOCATE( FRTB(3,NICRG,NICRG) )
      ALLOCATE( U0(3,4,NINDC) )
      ALLOCATE( RL0(3,NINDC) )
      ALLOCATE( IFRIC(10,NINDC) )
      ALLOCATE( ISTK(NICRG,NICRG,2) )
      ALLOCATE( FRIC(10,NINDC) )
      ALLOCATE( EDML(NICRG) )

      END
