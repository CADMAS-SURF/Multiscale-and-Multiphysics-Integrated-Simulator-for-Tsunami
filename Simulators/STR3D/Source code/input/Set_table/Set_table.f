      SUBROUTINE SET_TABLE( KK, ITO )
C
      USE INPUT_WORK
      USE M_VAL
C
      DIMENSION KK(*)
C
C     --- GRID ---
C
      DO I = 1, KK(8)
        IF( IGCD(I) > 0 ) IGCD(I) = ICRDR( IGCD(I) )
      ENDDO
C
C     --- ELEMENT & ELEMENT PROPERTY ---
C
      NSHL  = N_SHEL
      NSOL  = N_SOL
      NROD  = N_ROD
      NBAR  = N_BAR
      NELAS = N_ELS
      NELM  = N_SHEL + N_SOL + N_ROD + N_BAR + N_ELS
      NTHK  = N_PSHEL
      NRODA = N_PROD
      NBARD = N_PBAR
C
      CALL NM_COUNT( NM, J_SHEL, N_SHEL, J_SOL, N_SOL )
C
      KK(9)  = NSHL
      KK(10) = NSOL
      KK(12) = NELM
      KK(13) = NTHK
      KK(14) = NROD
      KK(15) = NRODA
      KK(16) = NBAR
      KK(17) = NBARD
      KK(18) = NELAS
      KK(37) = NM
C
      ALLOCATE( IELM(NM,NELM) )
      ALLOCATE( THK(NTHK) )
      ALLOCATE( RODA(NRODA) )
      ALLOCATE( BARD(6,NBARD) )
      ALLOCATE( BVEC(3,NBAR) )
      ALLOCATE( JELS(2,NELAS) )
      ALLOCATE( PELS(3,NELAS) )
      IELM(:,:) = 0
      THK(:) = 0.
      RODA(:) = 0.
      BARD(:,:) = 0.
      BVEC(:,:) = 0.
      JELS(:,:) = 0
      PELS(:,:) = 0.
C
      ALLOCATE( IELR(M_ELEM) )
      IELR(:) = 0
C
      CALL ST_ELEM( IELM, NM, NELM, THK, RODA, BARD, BVEC, JELS, PELS,
     &              IELR, GRID, KK(8), AMAT, J_PSHEL, R_PSHEL, N_PSHEL,
     &              J_PSOL, N_PSOL, J_PROD, R_PROD, N_PROD, J_PBAR,
     &              R_PBAR, N_PBAR, J_PELS, R_PELS, N_PELS, J_SHEL,
     &              N_SHEL, J_SOL, N_SOL, J_ROD, N_ROD, J_BAR,
     &              R_BAR, N_BAR, J_ELS, R_ELS, N_ELS, ICRDR, INDGR,
     &              IMATR, KK(1), ITO )
C
      DEALLOCATE( J_PSHEL )
      DEALLOCATE( R_PSHEL )
      DEALLOCATE( J_PSOL )
      DEALLOCATE( J_PROD )
      DEALLOCATE( R_PROD )
      DEALLOCATE( J_PBAR )
      DEALLOCATE( R_PBAR )
      DEALLOCATE( J_PELS )
      DEALLOCATE( R_PELS )
      DEALLOCATE( J_SHEL )
      DEALLOCATE( J_SOL )
      DEALLOCATE( J_ROD )
      DEALLOCATE( J_BAR )
      DEALLOCATE( R_BAR )
      DEALLOCATE( J_ELS )
      DEALLOCATE( R_ELS )
C
C     --- MATT1 ---
C
      ALLOCATE( MATT(5,KK(11)) )
      MATT(:,:) = 0
C
      CALL ST_MATT1( MATT, KK(11), J_MATT1, N_MATT1, IMATR, ITBM1R )
C
      DEALLOCATE( J_MATT1 )
C
C     --- MATS1 ---
C
      CALL ST_MATS1( MAT, AMAT, IMATR, J_MATS1, R_MATS1, N_MATS1 )
C
      DEALLOCATE( J_MATS1 )
      DEALLOCATE( R_MATS1 )
C
C     --- SPCD ---
C
      CALL SID_COUNT( NISPD, J_SPCD, 8, N_SPCD )
      NNSPD = N_SPCD
C
      KK(38) = NISPD
      KK(39) = NNSPD
C
      ALLOCATE( ISPD(2,NISPD) )
      ALLOCATE( NSPD(7,NNSPD) )
      ALLOCATE( SPCD(6,NNSPD) )
      ISPD(:,:) = 0
      NSPD(:,:) = 0
      SPCD(:,:) = 0.
C
      CALL ST_SID( ISPD, J_SPCD, 8, N_SPCD )
      CALL ST_SPC( ISPD, NSPD, SPCD, NISPD, NNSPD, J_SPCD, R_SPCD,
     &             N_SPCD, INDGR )
C
      DEALLOCATE( J_SPCD )
      DEALLOCATE( R_SPCD )
C
C     --- FORCE, MOMENT ---
C
      CALL SID_COUNT( NIFC, J_FRC, 3, N_FRC )
      NNFC = N_FRC
C
      KK(42) = NIFC
      KK(43) = NNFC
C
      ALLOCATE( IFC(2,NIFC) )
      ALLOCATE( NFC(2,NNFC) )
      ALLOCATE( FC(6,NNFC) )
      IFC(:,:) = 0
      NFC(:,:) = 0
      FC(:,:) = 0.
C
      CALL ST_SID( IFC, J_FRC, 3, N_FRC )
      CALL ST_FORCE( IFC, NFC, FC, NIFC, NNFC, J_FRC, R_FRC, N_FRC,
     &               ICRDR, INDGR )
C
      DEALLOCATE( J_FRC )
      DEALLOCATE( R_FRC )
C
C     --- PLOAD -> PLOAD4 ---
C
      CALL ST_PLOAD( J_PL4, R_PL4, N_PL4, J_PL, R_PL, N_PL, KK(12),
     &               IELM, KK(37), INDG, ITO )
C
      DEALLOCATE( J_PL )
      DEALLOCATE( R_PL )
C
C     --- PLOAD4 ---
C
      CALL SID_COUNT( NIPL4, J_PL4, 6, N_PL4 )
      CALL TB_COUNT( NNPL4, J_PL4, 6, N_PL4, IELR )
C
      KK(44) = NIPL4
      KK(45) = NNPL4
C
      ALLOCATE( IPL4(2,NIPL4) )
      ALLOCATE( NPL4(4,NNPL4) )
      ALLOCATE( PLD4(4,NNPL4) )
      IPL4(:,:) = 0
      NPL4(:,:) = 0
      PLD4(:,:) = 0.
C
      CALL ST_SID( IPL4, J_PL4, 6, N_PL4 )
      CALL ST_PLOAD4( IPL4, NPL4, PLD4, NIPL4, NNPL4, J_PL4, R_PL4,
     &                N_PL4, ICRDR, INDGR, IELR )
C
      DEALLOCATE( J_PL4 )
      DEALLOCATE( R_PL4 )
C
C     --- PLOAD1 ---
C
      CALL SID_COUNT( NIPL1, J_PL1, 3, N_PL1 )
      NNPL1 = N_PL1
C
      KK(74) = NIPL1
      KK(75) = NNPL1
C
      ALLOCATE( IPL1(2,NIPL1) )
      ALLOCATE( NPL1(NNPL1) )
      ALLOCATE( PLD1(3,NNPL1) )
      IPL1(:,:) = 0
      NPL1(:) = 0
      PLD1(:,:) = 0.
C
      CALL ST_SID( IPL1, J_PL1, 3, N_PL1 )
      CALL ST_PLOAD1( IPL1, NPL1, PLD1, NIPL1, NNPL1, J_PL1, R_PL1,
     &                N_PL1, IELR )
C
      DEALLOCATE( J_PL1 )
      DEALLOCATE( R_PL1 )
C
C     --- RFORCE ---
C
      IRFC(2,:) = INDGR( IRFC(2,:) )
C
C     --- TEMPD, TEMP ---
C
      CALL SID_COUNT2( NITMP, J_TMPD, N_TMPD, J_TMP, N_TMP )
C
      KK(58) = NITMP
C
      ALLOCATE( ITMP(NITMP) )
      ALLOCATE( TEMP(KK(8),NITMP) )
      ITMP(:) = 0
      TEMP(:,:) = 0.
C
      CALL ST_TEMP( ITMP, TEMP, NITMP, J_TMPD, R_TMPD, N_TMPD,
     &              J_TMP, R_TMP, N_TMP, INDGR, KK(8) )
C
      DEALLOCATE( J_TMPD )
      DEALLOCATE( R_TMPD )
      DEALLOCATE( J_TMP )
      DEALLOCATE( R_TMP )
C
C     --- TEMPP1 ---
C
      CALL SID_COUNT( NITP1, J_TMP1, 4, N_TMP1 )
C
      KK(59) = NITP1
C
      ALLOCATE( ITP1(NITP1) )
      ALLOCATE( ITPF(KK(12),NITP1) )
      ALLOCATE( TMP1(2,KK(12),NITP1) )
      ITP1(:) = 0
      ITPF(:,:) = 0
      TMP1(:,:,:) = 0.
C
      CALL ST_TEMPP1( ITP1, ITPF, TMP1, NITP1, J_TMP1, R_TMP1,
     &                N_TMP1, IELR, KK(12) )
C
      DEALLOCATE( J_TMP1 )
      DEALLOCATE( R_TMP1 )
C
C     --- SPC1 ---
C
      CALL SID_COUNT( NISP1, J_SPC1, 9, N_SPC1 )
      CALL TB_COUNT( NNSP1, J_SPC1, 9, N_SPC1, INDGR )
C
      KK(48) = NISP1
      KK(49) = NNSP1
C
      ALLOCATE( ISP1(2,NISP1) )
      ALLOCATE( NSP1(7,NNSP1) )
      ISP1(:,:) = 0
      NSP1(:,:) = 0
C
      CALL ST_SID( ISP1, J_SPC1, 9, N_SPC1 )
      CALL ST_SPC1( ISP1, NSP1, NISP1, NNSP1, J_SPC1, N_SPC1, INDGR )
C
      DEALLOCATE( J_SPC1 )
C
C     --- SPC ---
C
      CALL SID_COUNT( NISPC, J_SPC, 8, N_SPC )
      NNSPC = N_SPC
C
      KK(56) = NISPC
      KK(57) = NNSPC
C
      ALLOCATE( ISPC(2,NISPC) )
      ALLOCATE( NSPC(7,NNSPC) )
      ALLOCATE( SPC(6,NNSPC) )
      ISPC(:,:) = 0
      NSPC(:,:) = 0
      SPC(:,:) = 0.
C
      CALL ST_SID( ISPC, J_SPC, 8, N_SPC )
      CALL ST_SPC( ISPC, NSPC, SPC, NISPC, NNSPC, J_SPC, R_SPC,
     &             N_SPC, INDGR )
C
      DEALLOCATE( J_SPC )
      DEALLOCATE( R_SPC )
C
C     --- MPC ---
C
      CALL SID_COUNT( NIMPC, J_MPC, 3, N_MPC )
      NJMPC = N_MPC
      NNMPC = N2_MPC
C
      KK(52) = NIMPC
      KK(53) = NJMPC
      KK(54) = NNMPC
C
      ALLOCATE( IMPC(2,NIMPC) )
      ALLOCATE( JMPC(NJMPC) )
      ALLOCATE( NMPC(2,NNMPC) )
      ALLOCATE( AMPC(NNMPC) )
      IMPC(:,:) = 0
      JMPC(:) = 0
      NMPC(:,:) = 0
      AMPC(:) = 0.
C
      CALL ST_SID( IMPC, J_MPC, 3, N_MPC )
      CALL ST_MPC( IMPC, JMPC, NMPC, AMPC, NIMPC, NNMPC, J_MPC, J2_MPC,
     &             R2_MPC, N_MPC, INDGR )
C
      DEALLOCATE( J_MPC )
      DEALLOCATE( J2_MPC )
      DEALLOCATE( R2_MPC )
C
C     --- RLOAD1 ---
C
      IRL1(5,:) = ITBD1R( IRL1(5,:) )
      IRL1(6,:) = ITBD1R( IRL1(6,:) )
C
C     --- TLOAD1 ---
C
      ITL1(4,:) = ITBD1R( ITL1(4,:) )
C
C     --- BSURFS ---
C
      CALL SID_COUNT( NICRG, J_BSRF, 5, N_BSRF )
      NNCRG = N_BSRF
C
      KK(92) = NICRG
      KK(93) = NNCRG
C
      ALLOCATE( ICRG(2,NICRG) )
      ALLOCATE( NCRG(4,NNCRG) )
      ICRG(:,:) = 0
      NCRG(:,:) = 0
C
      ALLOCATE( ICRGR(M_BSRF) )
      ICRGR(:) = 0
C
      CALL ST_SID( ICRG, J_BSRF, 5, N_BSRF )
      CALL ST_BSURFS( ICRG, NCRG, ICRGR, NICRG, NNCRG, J_BSRF, N_BSRF,
     &                IELR, INDGR )
C
      DEALLOCATE( J_BSRF )
C
C     --- BCTSET ---
C
      CALL SID_COUNT( NICPR, J_BCTS, 3, N_BCTS )
      NNCPR = N_BCTS
C
      KK(90) = NICPR
      KK(91) = NNCPR
C
      ALLOCATE( ICPR(2,NICPR) )
      ALLOCATE( NCPR(2,NNCPR) )
      ICPR(:,:) = 0
      NCPR(:,:) = 0
C
      CALL ST_SID( ICPR, J_BCTS, 3, N_BCTS )
      CALL ST_BCTSET( ICPR, NCPR, NICPR, NNCPR, J_BCTS, N_BCTS, ICRGR )
C
      DEALLOCATE( J_BCTS )
C
C     --- BCTPARA ---
C
C
      ALLOCATE( CPR(2,NICPR) )
      CPR(:,:) = 0.
C
      CALL ST_BCTPARA( CPR, NICPR, ICPR, J_BCTP, R_BCTP, N_BCTP )
C
      DEALLOCATE( J_BCTP )
      DEALLOCATE( R_BCTP )
C
C     --- INVERSE NO. TABLE DEALLOCATE ---
C
      DEALLOCATE( ICRDR )
      DEALLOCATE( INDGR )
      DEALLOCATE( IELR )
      DEALLOCATE( IMATR )
      DEALLOCATE( ITBM1R )
      DEALLOCATE( ITBD1R )
      DEALLOCATE( ICRGR )
C
      END
