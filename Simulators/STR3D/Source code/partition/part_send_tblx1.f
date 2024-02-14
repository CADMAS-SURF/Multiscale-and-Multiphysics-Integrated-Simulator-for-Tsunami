      SUBROUTINE PART_SEND_TBLX1(KK,IP)

      USE M_VAL
      USE M_PART
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*),JG(5),IG(4096),IG0(4096)

      INTEGER, POINTER :: NODGX(:),IECX(:),IEGX(:),IEQX(:)
      INTEGER, POINTER :: IBN(:),IBG(:),IBF(:),IBE(:),IBS(:),IBELM(:)
      INTEGER, POINTER :: INDC0R(:),NS(:),ISL(:,:),NEP(:),INCX(:)
      INTEGER, POINTER :: LNOD(:),INDCW(:),IELCW(:,:),IEDGW(:,:)
      INTEGER, POINTER :: IELQW(:,:),LELQ(:),IFCQW(:),IEDQW(:),IVRQW(:)
      INTEGER, POINTER :: IELW(:),IELMW(:,:),IELMX(:,:),LELC(:),LEDG(:)
      INTEGER, POINTER :: ISLVW(:,:),IFRICW(:,:)
      REAL(8), POINTER :: RSLVW(:,:),FRICW(:,:),U0W(:,:,:),RL0W(:,:)
      REAL(8), POINTER :: DMTW(:,:),EPSGW(:,:),SIGGW(:,:),SIGYW(:,:)
      INTEGER, POINTER :: ISTW(:,:)
      REAL(8), POINTER :: DMTX(:,:),EPSGX(:,:),SIGGX(:,:),SIGYX(:,:)
      INTEGER, POINTER :: ISTX(:,:)

      NNOD = KK(8)
      NELM = KK(12)
      MGP = KK(36)
      NM = KK(37)
      IMATNL = KK(80)
      NIGSF = KK(94)
      NIELC = KK(97)
      NIEDG = KK(99)
      NIELQ = KK(101)
      NINDC0 = KK(102)

      NPECG = NPROCS - 1

!     --- INDC0R ---

      ALLOCATE( INDC0R(NNOD) )

      INDC0R(:) = 0

      DO I = 1, NINDC0
        INDC0R( INDC0(I) ) = I
      ENDDO

!     --- IBN ---

      ALLOCATE( IBN(NNOD) )

      IBN(:) = 0

      IBN( NOD(1:NN_INT(IP),IP) ) = 1

!     --- NS, IBS ---

      ALLOCATE( NS(NPECG) )
      ALLOCATE( IBS(NINDC0) )

      NS(:) = 0
      IBS(:) = 0

      DO I = 1, NINDC0

        IF( IBN( INDC0(I) ) == 1 ) CYCLE

        NN = 1

        IG(1) = INDC0(I)

        DO

          NN0 = 0

          DO J = 1, NN

            IGR = INDC0R( IG(J) )

            ISTS = ISLV0(1,IGR)
            MST  = ISLV0(2,IGR)
            MP   = ISLVP(IGR)

            IF( ISTS == 0 ) CYCLE

            SELECT CASE( ISTS )
            CASE( 1, 11 )
              IF( MST <= NN_INT(MP) ) THEN
                MSTG = NOD(MST,MP)
              ELSE
                MST = MST - ( NN_EXT(MP) + NN_EXTC(MP) )
                MSTG = NODG(MST,MP)
              ENDIF
              IF( IVRQ(MSTG) > 0 ) THEN
                N = 4
                JG(1:4) = IELQ(:,IVRQ(MSTG))
              ELSE
                N = 1
                JG(1) = MSTG
              ENDIF
            CASE( 2, 12, 14 )
              MSTG = IEG(MST,MP)
              IF( IEDQ(MSTG) > 0 ) THEN
                N = 4
                JG(1:4) = IELQ(:,IEDQ(MSTG))
              ELSE
                N = 2
                JG(1:2) = IEDG(1:2,MSTG)
              ENDIF
            CASE( 3, 13, 15 )
              MSTG = IEC(MST,MP)
              IF( IFCQ(MSTG) > 0 ) THEN
                N = 4
                JG(1:4) = IELQ(:,IFCQ(MSTG))
              ELSE
                N = 3
                JG(1:3) = IELC(:,MSTG)
              ENDIF
            END SELECT

            DO K = 1, N
              KG = JG(K)
              IF( IBN(KG) == 1 ) THEN
                KP = INDCP(1,I)
                NS(KP) = NS(KP) + 1
                IBS(I) = 1
                GOTO 10
              ENDIF
            ENDDO

            IG0(NN0+1:NN0+N) = JG(1:N)
            NN0 = NN0 + N

          ENDDO

          IF( NN0 == 0 ) EXIT

          NN = NN0

          IG(1:NN) = IG0(1:NN0)

        ENDDO

   10 ENDDO

!     --- ISL ---

      ALLOCATE( ISL(MAXVAL(NS),NPECG) )

      NS(:) = 0

      DO I = 1, NINDC0
        IF( IBS(I) == 1 ) THEN
          JP = INDCP(1,I)
          NS(JP) = NS(JP) + 1
          ISL(NS(JP),JP) = INDCP(2,I)
        ENDIF
      ENDDO

      DEALLOCATE( IBS )

!     --- SEND  NS, ISL ---

      CALL M_MPI_BCAST_I(NS,NPECG)

      DO JP = 1, NPECG
        IF( NS(JP) > 0 ) CALL M_MPI_SEND_I(ISL(1,JP),NS(JP),JP)
      ENDDO

      DEALLOCATE( ISL )

!     --- RECV  NEP, IELW, IELMW, GAUSS POINT DATA ---

      ALLOCATE( NEP(NPECG) )

      NEP(:) = 0

      DO JP = 1, NPECG
        IF( NS(JP) > 0 ) CALL M_MPI_RECV_I(NEP(JP),1,JP)
      ENDDO

      DEALLOCATE( NS )

      NEW = SUM(NEP)

      ALLOCATE( IELW(NEW) )
      ALLOCATE( IELMW(NM,NEW) )

      ALLOCATE( DMTW(21*MGP,NEW) )
      ALLOCATE( EPSGW(6*MGP,NEW) )
      ALLOCATE( SIGGW(6*MGP,NEW) )

      IF( IMATNL == 1 ) THEN
        ALLOCATE( ISTW(MGP,NEW) )
        ALLOCATE( SIGYW(MGP,NEW) )
      ENDIF

      IE = 0

      DO JP = 1, NPECG

        IF( NEP(JP) > 0 ) THEN

          IS = IE + 1
          IE = IE + NEP(JP)

          CALL M_MPI_RECV_I(IELW(IS),NEP(JP),JP)
          CALL M_MPI_RECV_I(IELMW(1,IS),NM*NEP(JP),JP)

          IELW(IS:IE) = IEL(IELW(IS:IE),JP)
          DO J = IS, IE
            N = IELMW(3,J)
            IELMW(8:7+N,J) = NOD(IELMW(8:7+N,J),JP)
          ENDDO

          CALL M_MPI_RECV_D(DMTW(1,IS),21*MGP*NEP(JP),JP)
          CALL M_MPI_RECV_D(EPSGW(1,IS),6*MGP*NEP(JP),JP)
          CALL M_MPI_RECV_D(SIGGW(1,IS),6*MGP*NEP(JP),JP)

          IF( IMATNL == 1 ) THEN
            CALL M_MPI_RECV_I(ISTW(1,IS),MGP*NEP(JP),JP)
            CALL M_MPI_RECV_D(SIGYW(1,IS),MGP*NEP(JP),JP)
          ENDIF

        ENDIF

      ENDDO

      DEALLOCATE( NEP )

!     --- IBELM ---

      ALLOCATE( IBELM(NELM) )

      IBELM(:) = 0

      IBELM( IEL(1:NE(IP),IP) ) = 1

!     --- IELMX, GAUSS POINT DATA ---

      ALLOCATE( IELMX(NM,NEW) )

      ALLOCATE( DMTX(21*MGP,NEW) )
      ALLOCATE( EPSGX(6*MGP,NEW) )
      ALLOCATE( SIGGX(6*MGP,NEW) )

      IF( IMATNL == 1 ) THEN
        ALLOCATE( ISTX(MGP,NEW) )
        ALLOCATE( SIGYX(MGP,NEW) )
      ENDIF

      NEX = 0

      DO I = 1, NEW

        J = IELW(I)

        IF( IBELM(J) == 0 ) THEN

          IBELM(J) = 1

          NEX = NEX + 1

          IELMX(:,NEX) = IELMW(:,I)

          DMTX(:,NEX) = DMTW(:,I)
          EPSGX(:,NEX) = EPSGW(:,I)
          SIGGX(:,NEX) = SIGGW(:,I)

          IF( IMATNL == 1 ) THEN
            ISTX(:,NEX) = ISTW(:,I)
            SIGYX(:,NEX) = SIGYW(:,I)
          ENDIF

        ENDIF

      ENDDO

      DEALLOCATE( IELW )
      DEALLOCATE( IELMW )

      DEALLOCATE( DMTW )
      DEALLOCATE( EPSGW )
      DEALLOCATE( SIGGW )

      IF( IMATNL == 1 ) THEN
        DEALLOCATE( ISTW )
        DEALLOCATE( SIGYW )
      ENDIF

      DEALLOCATE( IBELM )

!     --- IBG ---

      ALLOCATE( IBG(NNOD+NIGSF) )

      IBG(:) = 0

      IBG( NOD(1:NN_EXT(IP),IP) ) = 1
      IBG( NODG(1:NG(IP),IP) ) = 1

!     --- NODX ---

      NN_EXTX(IP) = 0

      DO I = 1, NEX
        N = IELMX(3,I)
        DO J = 1, N
          K = IELMX(7+J,I)
          IF( IBG(K) == 0 ) THEN
            IBG(K) = 1
            NN_EXTX(IP) = NN_EXTX(IP) + 1
            NODX(NN_EXTX(IP),IP) = K
          ENDIF
        ENDDO
      ENDDO

!     --- IBN, IBF, IBE ---

      ALLOCATE( IBF(NIELC) )
      ALLOCATE( IBE(NIEDG) )

      IBN(:) = 0
      IBF(:) = 0
      IBE(:) = 0

      IBN( NOD(1:NN_EXT(IP),IP) ) = 1
      IBN( NODX(1:NN_EXTX(IP),IP) ) = 1

      IBF( IEC(1:NIEC(IP),IP) ) = 1

      IBE( IEG(1:NIEG(IP),IP) ) = 1

!     --- NODX, NODGX, IECX, IEGX, IEQX ---

      ALLOCATE( NODGX(NINC(IP)) )
      ALLOCATE( IECX(NINC(IP)) )
      ALLOCATE( IEGX(NINC(IP)) )
      ALLOCATE( IEQX(NINC(IP)) )

      NGX = 0
      NIECX = 0
      NIEGX = 0
      NIEQX = 0

      DO I = 1, NINDC0

        IF( IBN( INDC0(I) ) == 0 ) CYCLE

        NN = 1

        IG(1) = INDC0(I)

        DO

          NN0 = 0

          DO J = 1, NN

            IGR = INDC0R( IG(J) )

            ISTS = ISLV0(1,IGR)
            MST  = ISLV0(2,IGR)
            MP   = ISLVP(IGR)

            IF( ISTS == 0 ) CYCLE

            SELECT CASE( ISTS )
            CASE( 1, 11 )
              IF( MST <= NN_INT(MP) ) THEN
                MSTG = NOD(MST,MP)
              ELSE
                MST = MST - ( NN_EXT(MP) + NN_EXTC(MP) )
                MSTG = NODG(MST,MP)
              ENDIF
              IF( IVRQ(MSTG) > 0 ) THEN
                N = 5
                JG(1:4) = IELQ(:,IVRQ(MSTG))
                JG(5) = MSTG
              ELSE
                N = 1
                JG(1) = MSTG
              ENDIF
            CASE( 2, 12, 14 )
              MSTG = IEG(MST,MP)
              IF( IBE(MSTG) == 1 ) CYCLE
              IBE(MSTG) = 1
              NIEGX = NIEGX + 1
              IEGX(NIEGX) = MSTG
              IF( IEDQ(MSTG) > 0 ) THEN
                N = 5
                JG(1:4) = IELQ(:,IEDQ(MSTG))
                JG(5) = IEDG(2,MSTG)
              ELSE
                N = 2
                JG(1:2) = IEDG(1:2,MSTG)
              ENDIF
            CASE( 3, 13, 15 )
              MSTG = IEC(MST,MP)
              IF( IBF(MSTG) == 1 ) CYCLE
              IBF(MSTG) = 1
              NIECX = NIECX + 1
              IECX(NIECX) = MSTG
              IF( IFCQ(MSTG) > 0 ) THEN
                N = 5
                JG(1:4) = IELQ(:,IFCQ(MSTG))
                JG(5) = IELC(3,MSTG)
              ELSE
                N = 3
                JG(1:3) = IELC(:,MSTG)
              ENDIF
            END SELECT

            DO K = 1, N
              KG = JG(K)
              IF( IBG(KG) == 1 ) CYCLE
              IBG(KG) = 1
              IF( KG <= NNOD ) THEN
                NN_EXTX(IP) = NN_EXTX(IP) + 1
                NODX(NN_EXTX(IP),IP) = KG
                NN0 = NN0 + 1
                IG0(NN0) = KG
              ELSE
                NGX = NGX + 1
                NODGX(NGX) = KG
                NIEQX = NIEQX + 1
                IEQX(NIEQX) = IVRQ(KG)
              ENDIF
            ENDDO

          ENDDO

          IF( NN0 == 0 ) EXIT

          NN = NN0

          IG(1:NN) = IG0(1:NN0)

        ENDDO

      ENDDO

!     --- IBN ---
      
      IBN(:) = 0

      IBN( NODX(1:NN_EXTX(IP),IP) ) = 1

!     --- INCX ---

      ALLOCATE( INCX(NN_EXTX(IP)) )

      NINCX = 0

      DO I = 1, NINDC0
        J = INDC0(I)
        IF( IBN(J) == 1 ) THEN
          NINCX = NINCX + 1
          INCX(NINCX) = I
        ENDIF
      ENDDO

      DEALLOCATE( INDC0R )

      DEALLOCATE( IBN )
      DEALLOCATE( IBF )
      DEALLOCATE( IBE )

      DEALLOCATE( IBG )

!     --- LNOD ---

      ALLOCATE( LNOD(NNOD+NIGSF) )

      LNOD(:) = 0

      DO I = 1, NN_EXT(IP)
        LNOD( NOD(I,IP) ) = I
      ENDDO

      DO I = 1, NG(IP)
        LNOD( NODG(I,IP) ) = NN_EXT(IP) + NN_EXTC(IP) + I
      ENDDO

      DO I = 1, NN_EXTX(IP)
        LNOD( NODX(I,IP) ) = NN_EXT(IP) + NN_EXTC(IP) + NG(IP) + NGC(IP)
     &                       + I
      ENDDO

      DO I = 1, NGX
        LNOD( NODGX(I) ) = NN_EXT(IP) + NN_EXTC(IP) + NG(IP) + NGC(IP)
     &                     + NN_EXTX(IP) + I
      ENDDO

!     --- IELM ---

      DO I = 1, NEX
        N = IELMX(3,I)
        IELMX(8:7+N,I) = LNOD( IELMX(8:7+N,I) )
      ENDDO

      CALL M_MPI_SEND_I(NEX,1,IP)
      IF( NEX > 0 ) CALL M_MPI_SEND_I(IELMX,NM*NEX,IP)

      DEALLOCATE( IELMX )

!     --- GAUSS POINT DATA ---

      IF( NEX > 0 ) THEN
        CALL M_MPI_SEND_D(DMTX,21*MGP*NEX,IP)
        CALL M_MPI_SEND_D(EPSGX,6*MGP*NEX,IP)
        CALL M_MPI_SEND_D(SIGGX,6*MGP*NEX,IP)
        IF( IMATNL == 1 ) THEN
          CALL M_MPI_SEND_I(ISTX,MGP*NEX,IP)
          CALL M_MPI_SEND_D(SIGYX,MGP*NEX,IP)
        ENDIF
      ENDIF

      DEALLOCATE( DMTX )
      DEALLOCATE( EPSGX )
      DEALLOCATE( SIGGX )

      IF( IMATNL == 1 ) THEN
        DEALLOCATE( ISTX )
        DEALLOCATE( SIGYX )
      ENDIF

!     --- GRID ---

      CALL M_MPI_SEND_I(NN_EXTX(IP),1,IP)
      CALL M_MPI_SEND_I(NGX,1,IP)

!     --- INDC ---

      ALLOCATE( INDCW(NINCX) )

      INDCW(1:NINCX) = LNOD( INDC0( INCX(1:NINCX) ) )

      CALL M_MPI_SEND_I(NINCX,1,IP)
      IF( NINCX > 0 ) CALL M_MPI_SEND_I(INDCW,NINCX,IP)

      DEALLOCATE( INDCW )

!     --- IELC ---

      ALLOCATE( IELCW(3,NIECX) )

      DO I = 1, NIECX
        IELCW(:,I) = LNOD( IELC(:,IECX(I)) )
      ENDDO

      CALL M_MPI_SEND_I(NIECX,1,IP)
      IF( NIECX > 0 ) CALL M_MPI_SEND_I(IELCW,3*NIECX,IP)

      DEALLOCATE( IELCW )

!     --- IEDG ---

      ALLOCATE( IEDGW(6,NIEGX) )

      DO I = 1, NIEGX
        IEDGW(1:2,I) = LNOD( IEDG(1:2,IEGX(I)) )
      ENDDO

      CALL M_MPI_SEND_I(NIEGX,1,IP)
      IF( NIEGX > 0 ) CALL M_MPI_SEND_I(IEDGW,6*NIEGX,IP)

      DEALLOCATE( IEDGW )

!     --- IELQ ---

      ALLOCATE( IELQW(4,NIEQX) )

      DO I = 1, NIEQX
        IELQW(:,I) = LNOD( IELQ(:,IEQX(I)) )
      ENDDO

      CALL M_MPI_SEND_I(NIEQX,1,IP)
      IF( NIEQX > 0 ) CALL M_MPI_SEND_I(IELQW,4*NIEQX,IP)

      DEALLOCATE( IELQW )

!     --- IFCQ, IEDQ, IVRQ ---

      ALLOCATE( LELQ(0:NIELQ) )

      LELQ(:) = 0

      DO I = 1, NIEQX
        LELQ( IEQX(I) ) = NIEQ(IP) + I
      ENDDO

      ALLOCATE( IFCQW(NIECX) )
      ALLOCATE( IEDQW(NIEGX) )
      ALLOCATE( IVRQW(NGX) )

      IFCQW(:) = LELQ( IFCQ( IECX(1:NIECX) ) )
      IEDQW(:) = LELQ( IEDQ( IEGX(1:NIEGX) ) )
      IVRQW(:) = LELQ( IVRQ( NODGX(1:NGX) ) )

      IF( NIECX > 0 ) CALL M_MPI_SEND_I(IFCQW,NIECX,IP)
      IF( NIEGX > 0 ) CALL M_MPI_SEND_I(IEDQW,NIEGX,IP)
      IF( NGX > 0 ) CALL M_MPI_SEND_I(IVRQW,NGX,IP)

      DEALLOCATE( LELQ )
      DEALLOCATE( IFCQW )
      DEALLOCATE( IEDQW )
      DEALLOCATE( IVRQW )

      DEALLOCATE( NODGX )
      DEALLOCATE( IEQX )

!     --- LELC, LEDG ---

      ALLOCATE( LELC(NIELC) )

      LELC(:) = 0

      DO I = 1, NIEC(IP)
        LELC( IEC(I,IP) ) = I
      ENDDO

      DO I = 1, NIECX
        LELC( IECX(I) ) = NIEC(IP) + I
      ENDDO

      ALLOCATE( LEDG(NIEDG) )

      LEDG(:) = 0

      DO I = 1, NIEG(IP)
        LEDG( IEG(I,IP) ) = I
      ENDDO

      DO I = 1, NIEGX
        LEDG( IEGX(I) ) = NIEG(IP) + I
      ENDDO

      DEALLOCATE( IECX )
      DEALLOCATE( IEGX )

!     --- ISLV, RSLV, IFRIC, FRIC, U0, RL0 ---

      NINDCW = NINC(IP) + NINCX

      ALLOCATE( ISLVW(2,NINDCW) )
      ALLOCATE( RSLVW(3,NINDCW) )
      ALLOCATE( IFRICW(10,NINDCW) )
      ALLOCATE( FRICW(10,NINDCW) )
      ALLOCATE( U0W(3,4,NINDCW) )
      ALLOCATE( RL0W(3,NINDCW) )

      ISLVW(:,:) = 0
      RSLVW(:,:) = 0.
      IFRICW(:,:) = 0
      FRICW(:,:) = 0.
      U0W(:,:,:) = 0.
      RL0W(:,:) = 0.

      DO I = 1, NINDCW

        IF( I <= NINC(IP) ) THEN
          J = INC(I,IP)
        ELSE
          J = INCX(I-NINC(IP))
        ENDIF

        ISTS = ISLV0(1,J)
        MST  = ISLV0(2,J)
        MP   = ISLVP(J)

        IF( ISTS == 0 ) CYCLE

        ISLVW(1,I) = ISTS

        SELECT CASE( ISTS )
        CASE( 1, 11 )
          IF( MST <= NN_INT(MP) ) THEN
            MSTG = NOD(MST,MP)
          ELSE
            MST = MST - ( NN_EXT(MP) + NN_EXTC(MP) )
            MSTG = NODG(MST,MP)
          ENDIF
          ISLVW(2,I) = LNOD(MSTG)
        CASE( 2, 12, 14 )
          MSTG = IEG(MST,MP)
          ISLVW(2,I) = LEDG(MSTG)
        CASE( 3, 13, 15 )
          MSTG = IEC(MST,MP)
          ISLVW(2,I) = LELC(MSTG)
        END SELECT

        RSLVW(:,I) = RSLV0(:,J)
        IFRICW(:,I) = IFRIC(:,J)
        FRICW(:,I) = FRIC(:,J)
        U0W(:,:,I) = U0(:,:,J)
        RL0W(:,I) = RL0(:,J)
        
      ENDDO

      IF( NINDCW > 0 ) THEN
        CALL M_MPI_SEND_I(ISLVW,2*NINDCW,IP)
        CALL M_MPI_SEND_D(RSLVW,3*NINDCW,IP)
        CALL M_MPI_SEND_I(IFRICW,10*NINDCW,IP)
        CALL M_MPI_SEND_D(FRICW,10*NINDCW,IP)
        CALL M_MPI_SEND_D(U0W,12*NINDCW,IP)
        CALL M_MPI_SEND_D(RL0W,3*NINDCW,IP)
      ENDIF

      DEALLOCATE( LELC )
      DEALLOCATE( LEDG )

      DEALLOCATE( ISLVW )
      DEALLOCATE( RSLVW )
      DEALLOCATE( IFRICW )
      DEALLOCATE( FRICW )
      DEALLOCATE( U0W )
      DEALLOCATE( RL0W )

      DEALLOCATE( LNOD )

      DEALLOCATE( INCX )

      END
