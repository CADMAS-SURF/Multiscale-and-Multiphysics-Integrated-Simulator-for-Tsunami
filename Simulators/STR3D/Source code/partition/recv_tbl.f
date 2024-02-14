      SUBROUTINE RECV_TBL(KK)

      USE M_VAL
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*)

!     --- GRID ---

      CALL M_MPI_RECV_I(NNODI,1,0)
      CALL M_MPI_RECV_I(NNOD,1,0)
      CALL M_MPI_RECV_I(NNODC,1,0)
      CALL M_MPI_RECV_I(NIGSF,1,0)
      CALL M_MPI_RECV_I(NIGSFC,1,0)

      KK(26) = NNODI
      KK(8) = NNOD
      KK(28) = NNODC
      KK(94) = NIGSF
      KK(108) = NIGSFC

      ALLOCATE( INDG(NNOD+NNODC) )
      ALLOCATE( GRID(3,NNOD+NNODC) )

      CALL M_MPI_RECV_I(INDG,NNOD+NNODC,0)
      CALL M_MPI_RECV_D(GRID,3*(NNOD+NNODC),0)

!     --- ELEMENT ---

      NM = KK(37)

      CALL M_MPI_RECV_I(NELM,1,0)
      CALL M_MPI_RECV_I(NELMC,1,0)

      KK(12) = NELM
      KK(29) = NELMC

      ALLOCATE( IELM(NM,NELM+NELMC) )

      CALL M_MPI_RECV_I(IELM,NM*(NELM+NELMC),0)

      NSOL = 0
      NROD = 0

      DO I = 1, NELM
        SELECT CASE( IELM(2,I) )
        CASE( 2, 6 )
          NSOL = NSOL + 1
        CASE( 3 )
          NROD = NROD + 1
        END SELECT
      ENDDO

      KK(10) = NSOL
      KK(14) = NROD

!     --- SPCD ---

      IF( KK(38) > 0 ) THEN

        CALL M_MPI_RECV_I(NISPD,1,0)
        CALL M_MPI_RECV_I(NNSPD,1,0)

        KK(38) = NISPD
        KK(39) = NNSPD

        IF( NISPD > 0 ) THEN
          ALLOCATE( ISPD(2,NISPD) )
          ALLOCATE( NSPD(7,NNSPD) )
          ALLOCATE( SPCD(6,NNSPD) )
          CALL M_MPI_RECV_I(ISPD,2*NISPD,0)
          CALL M_MPI_RECV_I(NSPD,7*NNSPD,0)
          CALL M_MPI_RECV_D(SPCD,6*NNSPD,0)
        ENDIF

      ENDIF

!     --- FORCE ---

      IF( KK(42) > 0 ) THEN

        CALL M_MPI_RECV_I(NIFC,1,0)
        CALL M_MPI_RECV_I(NNFC,1,0)

        KK(42) = NIFC
        KK(43) = NNFC

        IF( NIFC > 0 ) THEN
          ALLOCATE( IFC(2,NIFC) )
          ALLOCATE( NFC(2,NNFC) )
          ALLOCATE( FC(6,NNFC) )
          CALL M_MPI_RECV_I(IFC,2*NIFC,0)
          CALL M_MPI_RECV_I(NFC,2*NNFC,0)
          CALL M_MPI_RECV_D(FC,6*NNFC,0)
        ENDIF

      ENDIF

!     --- PLOAD4 ---

      IF( KK(44) > 0 ) THEN

        CALL M_MPI_RECV_I(NIPL4,1,0)
        CALL M_MPI_RECV_I(NNPL4,1,0)

        KK(44) = NIPL4
        KK(45) = NNPL4

        IF( NIPL4 > 0 ) THEN
          ALLOCATE( IPL4(2,NIPL4) )
          ALLOCATE( NPL4(4,NNPL4) )
          ALLOCATE( PLD4(4,NNPL4) )
          CALL M_MPI_RECV_I(IPL4,2*NIPL4,0)
          CALL M_MPI_RECV_I(NPL4,4*NNPL4,0)
          CALL M_MPI_RECV_D(PLD4,4*NNPL4,0)
        ENDIF

      ENDIF

!     --- SPC1 ---

      IF( KK(48) > 0 ) THEN

        CALL M_MPI_RECV_I(NISP1,1,0)
        CALL M_MPI_RECV_I(NNSP1,1,0)

        KK(48) = NISP1
        KK(49) = NNSP1

        IF( NISP1 > 0 ) THEN
          ALLOCATE( ISP1(2,NISP1) )
          ALLOCATE( NSP1(7,NNSP1) )
          CALL M_MPI_RECV_I(ISP1,2*NISP1,0)
          CALL M_MPI_RECV_I(NSP1,7*NNSP1,0)
        ENDIF

      ENDIF

!     --- SPC ---

      IF( KK(56) > 0 ) THEN

        CALL M_MPI_RECV_I(NISPC,1,0)
        CALL M_MPI_RECV_I(NNSPC,1,0)

        KK(56) = NISPC
        KK(57) = NNSPC

        IF( NISPC > 0 ) THEN
          ALLOCATE( ISPC(2,NISPC) )
          ALLOCATE( NSPC(7,NNSPC) )
          ALLOCATE( SPC(6,NNSPC) )
          CALL M_MPI_RECV_I(ISPC,2*NISPC,0)
          CALL M_MPI_RECV_I(NSPC,7*NNSPC,0)
          CALL M_MPI_RECV_D(SPC,6*NNSPC,0)
        ENDIF

      ENDIF

!     --- IMPORT & EXPORT NODE ---

      CALL M_MPI_RECV_I(NPE,1,0)
      CALL M_MPI_RECV_I(NIMP,1,0)
      CALL M_MPI_RECV_I(NEXP,1,0)

      ALLOCATE( IPE(NPE) )
      ALLOCATE( IDXIMP(2,NPE) )
      ALLOCATE( NODIMP(NIMP) )
      ALLOCATE( IDXEXP(2,NPE) )
      ALLOCATE( NODEXP(NEXP) )

      CALL M_MPI_RECV_I(IPE,NPE,0)
      CALL M_MPI_RECV_I(IDXIMP,2*NPE,0)
      CALL M_MPI_RECV_I(NODIMP,NIMP,0)
      CALL M_MPI_RECV_I(IDXEXP,2*NPE,0)
      CALL M_MPI_RECV_I(NODEXP,NEXP,0)

      IF( KK(92) == 0 ) RETURN

!     --- IBEL ---

      ALLOCATE( IBEL(NELM+NELMC) )

      CALL M_MPI_RECV_I(IBEL,NELM+NELMC,0)

!     --- IBTE ---

      CALL M_MPI_RECV_I(NIBTE,1,0)

      KK(100) = NIBTE

      IF( NIBTE > 0 ) THEN
        ALLOCATE( IBTE(4,NIBTE) )
        CALL M_MPI_RECV_I(IBTE,4*NIBTE,0)
      ENDIF

!     --- IELC ---

      NICRG = KK(92)

      ALLOCATE( ICBD(NICRG) )
      CALL M_MPI_RECV_I(ICBD,NICRG,0)

      CALL M_MPI_RECV_I(NIELC,1,0)

      KK(97) = NIELC

      IF( NIELC > 0 ) THEN
        ALLOCATE( IELC(3,NIELC) )
        CALL M_MPI_RECV_I(IELC,3*NIELC,0)
      ENDIF

!     --- IELCB ---

      IF( NIELC > 0 ) THEN
        ALLOCATE( IELCB(NIELC) )
        CALL M_MPI_RECV_I(IELCB,NIELC,0)
      ENDIF

!     --- INDA0 ---

      ALLOCATE( INDA0(NICRG) )
      CALL M_MPI_RECV_I(INDA0,NICRG,0)

!     --- INDC ---

      ALLOCATE( INDA(NICRG) )
      CALL M_MPI_RECV_I(INDA,NICRG,0)

      CALL M_MPI_RECV_I(NINDC,1,0)

      KK(95) = NINDC

      IF( NINDC > 0 ) THEN
        ALLOCATE( INDC(NINDC) )
        CALL M_MPI_RECV_I(INDC,NINDC,0)
      ENDIF

!     --- IEDG ---

      ALLOCATE( IEDA(NICRG) )
      CALL M_MPI_RECV_I(IEDA,NICRG,0)

      CALL M_MPI_RECV_I(NIEDG,1,0)

      KK(99) = NIEDG

      IF( NIEDG > 0 ) THEN
        ALLOCATE( IEDG(6,NIEDG) )
        CALL M_MPI_RECV_I(IEDG,6*NIEDG,0)
      ENDIF

!     --- IEDGB ---

      IF( NIEDG > 0 ) THEN
        ALLOCATE( IEDGB(NIEDG) )
        CALL M_MPI_RECV_I(IEDGB,NIEDG,0)
      ENDIF

!     --- IELG ---

      CALL M_MPI_RECV_I(NIELG,1,0)

      KK(98) = NIELG

      IF( NIELG > 0 ) THEN
        ALLOCATE( IELG(NIELG) )
        CALL M_MPI_RECV_I(IELG,NIELG,0)
      ENDIF

!     --- IELQ ---

      CALL M_MPI_RECV_I(NIELQ,1,0)

      KK(101) = NIELQ

      IF( NIELQ > 0 ) THEN
        ALLOCATE( IELQ(4,NIELQ) )
        CALL M_MPI_RECV_I(IELQ,4*NIELQ,0)
      ENDIF

!     ---- IFCQ, IEDQ, IVRQ ---

      IF( NIELC > 0 ) THEN
        ALLOCATE( IFCQ(NIELC) )
        CALL M_MPI_RECV_I(IFCQ,NIELC,0)
      ENDIF

      IF( NIEDG > 0 ) THEN
        ALLOCATE( IEDQ(NIEDG) )
        CALL M_MPI_RECV_I(IEDQ,NIEDG,0)
      ENDIF

      IF( NIGSF > 0 ) THEN
        ALLOCATE( IVRQ(NNOD+NNODC+NIGSF) )
        IVRQ(:) = 0
        CALL M_MPI_RECV_I(IVRQ(NNOD+NNODC+1),NIGSF,0)
      ENDIF

      END
