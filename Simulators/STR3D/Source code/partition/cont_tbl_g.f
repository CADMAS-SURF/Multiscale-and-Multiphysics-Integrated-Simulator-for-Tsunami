      SUBROUTINE CONT_TBL_G(KK,ITO)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER*8 MUSED
      DIMENSION KK(*),NP(8),IG(2),NOD(2,3)
      INTEGER, POINTER :: IFIL(:),NED(:),IED(:,:,:),IEP(:,:),IEDGW(:,:)
      INTEGER, POINTER :: IEDQW(:)
      DATA NOD / 2,3, 3,1, 1,2 /

      WRITE(ITO,'(/X,A,F8.1,A)')
     &  'SUB. CONT_TBL_G : ', MUSED()*4.D-6, '(MB) USED.'

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

      KK(101) = NIELQ

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

      KK(100) = NIBTE

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

!     --- INDA0, INDC0 ---

      ALLOCATE( INDA0(NICRG) )

      ALLOCATE( IFIL(NNOD) )

      IFIL(:) = 0

      DO I = 1, NNCRG
        N = ICSF(2,I)
        IFIL( ICSF(3:2+N,I) ) = 1
      ENDDO

      NINDC0 = SUM(IFIL)

      KK(102) = NINDC0

      ALLOCATE( INDC0(NINDC0) )

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
            INDC0(IP) = JG
            IFIL(JG) = 1
          ENDDO
        ENDDO

        INDA0(IRG) = IP

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

      KK(99) = NIEDG

      ALLOCATE( IEDG(6,NIEDG) )
      ALLOCATE( IEDQ(NIEDG) )

      IEDG(:,:)=IEDGW(:,1:NIEDG)
      IEDQ(:)=IEDQW(1:NIEDG)

      DEALLOCATE( NED )
      DEALLOCATE( IED )
      DEALLOCATE( IEP )

      DEALLOCATE( IEDGW )
      DEALLOCATE( IEDQW )

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

!     --- ISLV0, ISLVP ---

      ALLOCATE( ISLV0(2,NINDC0) )
      ALLOCATE( ISLVP(NINDC0) )

!     --- FRTB, ISTK, EDML ---

      ALLOCATE( FRTB(3,NICRG,NICRG) )
      ALLOCATE( ISTK(NICRG,NICRG,2) )
      ALLOCATE( EDML(NICRG) )

      END
