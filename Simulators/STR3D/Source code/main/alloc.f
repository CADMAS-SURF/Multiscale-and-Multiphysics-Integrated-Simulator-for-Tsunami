      SUBROUTINE ALLOC(KK)

      USE M_VAL
      USE MPI_PARAM

      DIMENSION KK(*)

      NNOD = KK(8)
      NELM = KK(12)
      NBAR = KK(16)
      NNODC = KK(28)
      NIGSF = KK(94)
      NIGSFC = KK(108)

      MGP = 0

      DO I = 1, NELM

        ITYP = IELM(2,I)
        NNP = IELM(3,I)

        SELECT CASE( ITYP )
        CASE( 2, 6 )
          SELECT CASE( NNP )
          CASE( 4 )
            NGP = 1
          CASE( 10 )
            NGP = 5
          CASE( 6 )
            NGP = 6
          CASE( 15 )
            NGP = 21
          CASE( 8 )
            NGP = 8
          CASE( 20 )
            NGP = 27
          END SELECT
        CASE( 3 ) 
          NGP = 2
        CASE( 4 )
          NGP = 9
        END SELECT

        IF( NGP > MGP ) MGP = NGP

      ENDDO

      IF( MYRANK > 0 ) THEN
        CALL CG_MPI_ALLREDUCE_I(MGP,MAX,1,1)
        MGP = MAX
      ENDIF

      KK(36) = MGP

      IF( MYRANK == 1 ) CALL SEND_KK(KK,36)

      ALLOCATE( UG1(6,NNOD+NNODC+NIGSF) )
      ALLOCATE( UG2(6,NNOD+NNODC+NIGSF) )
      ALLOCATE( UG3(6,NNOD+NNODC+NIGSF) )
      ALLOCATE( UGP(6,NNOD+NNODC+NIGSF) )
      ALLOCATE( DUG(6,NNOD) )
      ALLOCATE( POS(3,NNOD+NNODC+NIGSF+NIGSFC) )
      ALLOCATE( POSO(3,NNOD+NNODC+NIGSF+NIGSFC) )
      ALLOCATE( VG0(3,2,2,NBAR) )
      ALLOCATE( VG(3,2,2,NBAR) )
      ALLOCATE( VGP(3,2,2,NBAR) )
      ALLOCATE( FTO(6,NNOD) )
      ALLOCATE( FCO(6,NNOD,3) )
      ALLOCATE( FTI(6,NNOD) )
      ALLOCATE( FTID(6,NNOD) )
      ALLOCATE( FCK(6,NNOD,3) )
      ALLOCATE( FCD(6,NNOD,4) )
      ALLOCATE( FCM(6,NNOD,3) )
      ALLOCATE( FCMD(6,NNOD,3) )
      ALLOCATE( RFCO(6,NNOD) )
      ALLOCATE( RFCI(6,NNOD) )
      ALLOCATE( FRCI(3,NNOD) )

      ALLOCATE( WRK1(6,NNOD) )
      ALLOCATE( WRK2(6,NNOD+NIGSF) )
      ALLOCATE( WRK3(6,NNOD) )

      ALLOCATE( DMT(21*MGP,NELM) )
      ALLOCATE( EPSG(6*MGP,NELM) )
      ALLOCATE( SIGG(6*MGP,NELM) )

      ALLOCATE( INDOF0(6,NNOD) )
      ALLOCATE( INDOF(6,NNOD) )
      ALLOCATE( INDMPC(2,6,NNOD) )

      ALLOCATE( PPND(NNOD) )

      ALLOCATE( FCP(3,NNOD,2) )

      IF( KK(25) == 0 ) RETURN

      ALLOCATE( PG1(NNOD+NNODC+NIGSF) )
      ALLOCATE( PG2(NNOD+NNODC+NIGSF) )
      ALLOCATE( PG3(NNOD+NNODC+NIGSF) )
      ALLOCATE( DPG(NNOD) )
      ALLOCATE( FLO(NNOD) )
      ALLOCATE( FLI(NNOD) )

      ALLOCATE( VELG(3,MGP,NELM) )
      ALLOCATE( VELE(3,NELM) )

      ALLOCATE( INDOP0(NNOD) )
      ALLOCATE( INDOP(NNOD) )
      
      END