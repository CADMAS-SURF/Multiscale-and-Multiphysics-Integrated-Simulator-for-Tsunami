      SUBROUTINE COMM_TBLX(KK)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*)

      INTEGER, POINTER :: IWK1(:),IWK2(:,:)
      REAL(8), POINTER :: WK1(:),WK2(:,:),WK3(:,:,:)

      NNOD = KK(8)
      NELM = KK(12)
      NNODI = KK(26)
      NNODC = KK(28)
      NELMC = KK(29)
      NNODX = KK(31)
      NELMX = KK(32)
      MGP = KK(36)
      NIGSF = KK(94)
      NIELQ = KK(101)
      NIGSFX = KK(107)
      NIGSFC = KK(108)

      N0 = NNODI
      N1 = NNOD
      N2 = NNOD + NNODC
      N3 = NNOD + NNODC + NIGSF
      N4 = NNOD + NNODC + NIGSF + NIGSFC + NNODX
      N5 = NNOD + NNODC + NIGSF + NIGSFC + NNODX + NIGSFX

      NE1 = NELM
      NE2 = NELM + NELMC
      NE3 = NELM + NELMC + NELMX

!     --- INDG ---

      ALLOCATE( IWK1(N2) )
      IWK1(:) = INDG(1:N2)
      DEALLOCATE( INDG )
      ALLOCATE( INDG(N4) )
      INDG(1:N2) = IWK1(:)
      DEALLOCATE( IWK1 )

      CALL COMM_INTX(INDG,1)

!     --- GRID ---

      ALLOCATE( WK2(3,N2) )
      WK2(:,:) = GRID(:,1:N2)
      DEALLOCATE( GRID )
      ALLOCATE( GRID(3,N4) )
      GRID(:,1:N2) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBLX(GRID,3)

!     --- UG1 ---

      ALLOCATE( WK2(6,N1) )
      WK2(:,:) = UG1(:,1:N1)
      DEALLOCATE( UG1 )
      ALLOCATE( UG1(6,N5) )
      UG1(:,1:N1) = WK2(:,:)
      DEALLOCATE( WK2 )

!     --- UG2 ---

      ALLOCATE( WK2(6,N1) )
      WK2(:,:) = UG2(:,1:N1)
      DEALLOCATE( UG2 )
      ALLOCATE( UG2(6,N5) )
      UG2(:,1:N1) = WK2(:,:)
      DEALLOCATE( WK2 )

!     --- UG3 ---

      ALLOCATE( WK2(6,N3) )
      WK2(:,:) = UG3(:,1:N3)
      DEALLOCATE( UG3 )
      ALLOCATE( UG3(6,N5) )
      UG3(:,1:N3) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBLX(UG3,6)

      CALL GSURFX(UG3(1,N4+1),6,NIGSFX,UG3,IELQ(1,NIELQ+1))

!     --- UGP ---

      DEALLOCATE( UGP )
      ALLOCATE( UGP(6,N5) )

!     --- DUG ---

      ALLOCATE( WK2(6,N1) )
      WK2(:,:) = DUG(:,1:N1)
      DEALLOCATE( DUG )
      ALLOCATE( DUG(6,N4) )
      DUG(:,1:N1) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBLX(DUG,6)

!     --- POS ---

      ALLOCATE( WK2(3,N3) )
      WK2(:,:) = POS(:,1:N3)
      DEALLOCATE( POS )
      ALLOCATE( POS(3,N5) )
      POS(:,1:N3) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBLX(POS,3)

      CALL GSURFX(POS(1,N4+1),3,NIGSFX,POS,IELQ(1,NIELQ+1))

!     --- FTO ---

      ALLOCATE( WK2(6,N0) )
      WK2(:,:) = FTO(:,1:N0)
      DEALLOCATE( FTO )
      ALLOCATE( FTO(6,N4) )
      FTO(:,1:N0) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBL(FTO,6)
      CALL COMM_DBLX(FTO,6)

!     --- FTI ---

      DEALLOCATE( FTI )
      ALLOCATE( FTI(6,N4) )

!     --- FTID ---

      ALLOCATE( WK2(6,N1) )
      WK2(:,:) = FTID(:,1:N1)
      DEALLOCATE( FTID )
      ALLOCATE( FTID(6,N4) )
      FTID(:,1:N1) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBLX(FTID,6)

!     --- FCK ---

      ALLOCATE( WK3(6,N0,2) )
      WK3(:,:,:) = FCK(:,1:N0,1:2)
      DEALLOCATE( FCK )
      ALLOCATE( FCK(6,N4,3) )
      FCK(:,1:N0,1:2) = WK3(:,:,:)
      DEALLOCATE( WK3 )

      CALL COMM_DBL(FCK(1,1,1),6)
      CALL COMM_DBLX(FCK(1,1,1),6)
      CALL COMM_DBL(FCK(1,1,2),6)
      CALL COMM_DBLX(FCK(1,1,2),6)

!     --- FCD ---

      ALLOCATE( WK3(6,N0,3) )
      WK3(:,:,:) = FCD(:,1:N0,1:3)
      DEALLOCATE( FCD )
      ALLOCATE( FCD(6,N4,4) )
      FCD(:,1:N0,1:3) = WK3(:,:,:)
      DEALLOCATE( WK3 )

      CALL COMM_DBL(FCD(1,1,1),6)
      CALL COMM_DBLX(FCD(1,1,1),6)
      CALL COMM_DBL(FCD(1,1,2),6)
      CALL COMM_DBLX(FCD(1,1,2),6)
      CALL COMM_DBL(FCD(1,1,3),6)
      CALL COMM_DBLX(FCD(1,1,3),6)

!     --- FCM ---

      ALLOCATE( WK3(6,N0,2) )
      WK3(:,:,:) = FCM(:,1:N0,1:2)
      DEALLOCATE( FCM )
      ALLOCATE( FCM(6,N4,3) )
      FCM(:,1:N0,1:2) = WK3(:,:,:)
      DEALLOCATE( WK3 )

      CALL COMM_DBL(FCM(1,1,1),6)
      CALL COMM_DBLX(FCM(1,1,1),6)
      CALL COMM_DBL(FCM(1,1,2),6)
      CALL COMM_DBLX(FCM(1,1,2),6)

!     --- FCMD ---

      ALLOCATE( WK3(6,N0,2) )
      WK3(:,:,:) = FCMD(:,1:N0,1:2)
      DEALLOCATE( FCMD )
      ALLOCATE( FCMD(6,N4,3) )
      FCMD(:,1:N0,1:2) = WK3(:,:,:)
      DEALLOCATE( WK3 )

      CALL COMM_DBL(FCMD(1,1,1),6)
      CALL COMM_DBLX(FCMD(1,1,1),6)
      CALL COMM_DBL(FCMD(1,1,2),6)
      CALL COMM_DBLX(FCMD(1,1,2),6)

!     --- FCP ---

      ALLOCATE( WK2(3,N0) )
      WK2(:,:) = FCP(:,1:N0,1)
      DEALLOCATE( FCP )
      ALLOCATE( FCP(3,N4,2) )
      FCP(:,1:N0,1) = WK2(:,:)
      DEALLOCATE( WK2 )

      CALL COMM_DBL(FCP,3)
      CALL COMM_DBLX(FCP,3)

!     --- RFCI ---

      DEALLOCATE( RFCI )
      ALLOCATE( RFCI(6,N4) )

!     --- FRCI ---

      DEALLOCATE( FRCI )
      ALLOCATE( FRCI(3,N4) )

!     --- PPND ---

      ALLOCATE( WK1(N1) )
      WK1(:) = PPND(1:N1)
      DEALLOCATE( PPND )
      ALLOCATE( PPND(N4) )
      PPND(1:N1) = WK1(:)
      DEALLOCATE( WK1 )

      CALL COMM_DBLX(PPND,1)

!     --- WRK1 ---

      DEALLOCATE( WRK1 )
      ALLOCATE( WRK1(6,N4) )

!     --- INDOF0 ---

      ALLOCATE( IWK2(6,N1) )
      IWK2(:,:) = INDOF0(:,1:N1)
      DEALLOCATE( INDOF0 )
      ALLOCATE( INDOF0(6,N4) )
      INDOF0(:,1:N1) = IWK2(:,:)
      DEALLOCATE( IWK2 )

      CALL COMM_INTX(INDOF0,6)

!     --- INDOF ---

      DEALLOCATE( INDOF )
      ALLOCATE( INDOF(6,N4) )

!     --- INDMPC ---

      DEALLOCATE( INDMPC )
      ALLOCATE( INDMPC(2,6,N4) )

      IF( KK(21) <= 4 ) RETURN

!     --- PG1 ---

      ALLOCATE( WK1(N1) )
      WK1(:) = PG1(1:N1)
      DEALLOCATE( PG1 )
      ALLOCATE( PG1(N4) )
      PG1(1:N1) = WK1(:)
      DEALLOCATE( WK1 )

      CALL COMM_DBLX(PG1,1)

!     --- PG2 ---

      ALLOCATE( WK1(N1) )
      WK1(:) = PG2(1:N1)
      DEALLOCATE( PG2 )
      ALLOCATE( PG2(N4) )
      PG2(1:N1) = WK1(:)
      DEALLOCATE( WK1 )

      CALL COMM_DBLX(PG2,1)

!     --- PG3 ---

      ALLOCATE( WK1(N1) )
      WK1(:) = PG3(1:N1)
      DEALLOCATE( PG3 )
      ALLOCATE( PG3(N4) )
      PG3(1:N1) = WK1(:)
      DEALLOCATE( WK1 )

      CALL COMM_DBLX(PG3,1)

!     --- DPG ---

      ALLOCATE( WK1(N1) )
      WK1(:) = DPG(1:N1)
      DEALLOCATE( DPG )
      ALLOCATE( DPG(N4) )
      DPG(1:N1) = WK1(:)
      DEALLOCATE( WK1 )

      CALL COMM_DBLX(DPG,1)

!     --- FLO ---

      DEALLOCATE( FLO )
      ALLOCATE( FLO(N4) )

!     --- FLI ---

      DEALLOCATE( FLI )
      ALLOCATE( FLI(N4) )

!     --- VELG ---

      DEALLOCATE( VELG )
      ALLOCATE( VELG(3,MGP,NE3) )

!     --- VELE ---

      DEALLOCATE( VELE )
      ALLOCATE( VELE(3,NE3) )

!     --- INDOP0 ---

      ALLOCATE( IWK1(N1) )
      IWK1(:) = INDOP0(1:N1)
      DEALLOCATE( INDOP0 )
      ALLOCATE( INDOP0(N4) )
      INDOP0(1:N1) = IWK1(:)
      DEALLOCATE( IWK1 )

      CALL COMM_INTX(INDOP0,1)

!     --- INDOP ---

      DEALLOCATE( INDOP )
      ALLOCATE( INDOP(N4) )

      END
