      SUBROUTINE READ_DATA( KK, RR, ITI, ITO )
C
      USE INPUT_WORK
      USE M_VAL
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION KK(*), RR(*)
C
C     --- SOL, SUBCASE ---
C
C      NSUB = N_SUB
      NSUB = 1
C
      KK(4) = NSUB
C
      ALLOCATE( ISUB(13,NSUB) )
      ISUB(:,:) = 0
C
      I_SUB = 0
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:10) == 'BEGIN BULK' ) THEN
C
          EXIT
C
        ELSEIF( CHAR(1:3) == 'SOL' ) THEN
C
C          READ(CHAR(4:80),*) ISOL
C
C          IF( ISOL == 101 ) THEN
C            KK(1) = 0
C          ELSEIF( ISOL == 103 ) THEN
C            KK(1) = 1
C          ELSEIF( ISOL == 109 ) THEN
C            KK(1) = 2
C          ELSEIF( ISOL == 108 ) THEN
C            KK(1) = 3
C          ELSEIF( ISOL == 106 ) THEN
C            KK(1) = 4
C          ELSEIF( ISOL == 129 ) THEN
C            KK(1) = 5
C          ENDIF
C
C        ELSEIF( CHAR(1:7) == 'SUBCASE' ) THEN
C
          I_SUB = I_SUB + 1
          CALL RD_SUBCASE( ISUB(1,I_SUB), ITI )
C
        ELSE
C
          CALL GET_COMM( IP, IS, IE, CHAR )
C
          IF( IP == 0 ) CYCLE
C
          IF( CHAR(IS:IE) == 'TEMPERATURE(INITIAL)' ) THEN
            READ(CHAR(IP+1:80),*) ITMPI
            ISUB( 6,:) = ITMPI
            ISUB(10,:) = ITMPI
          ELSEIF( CHAR(IS:IE) == 'LOADSET' ) THEN
            READ(CHAR(IP+1:80),*) ILDSET
            ISUB(8,:) = ILDSET
          ENDIF
C
        ENDIF
C
      ENDDO
C
C     ---- REST ---
C
      NRST = N_RST
C
      KK(83) = NRST
C
      ALLOCATE( RTIM(2+NRST) )
      RTIM(:) = 0.
C
      I_RST = 0
C
C     ---- CORD2C, CORD2R ---
C
      NICRD = N_CORD
C
      KK(64) = NICRD
C
      ALLOCATE( ICRD(NICRD) )
      ALLOCATE( CORD(9,NICRD) )
      ICRD(:) = 0
      CORD(:,:) = 0.
C
      ALLOCATE( ICRDR(M_CORD) )
      ICRDR(:) = 0
C
      I_CORD = 0
C
C     ---- GRID ---
C
      NNOD = N_GRID
C
      KK(8) = NNOD
      KK(26) = NNOD
C
      ALLOCATE( INDG(NNOD) )
      ALLOCATE( IGCD(NNOD) )
      ALLOCATE( GRID(3,NNOD + N_SHEL) )
      INDG(:) = 0
      IGCD(:) = 0
      GRID(:,:) = 0.
C
      ALLOCATE( INDGR(M_GRID) )
      INDGR(:) = 0
C
      I_GRID = 0
C
C     --- CTRIA3, CTRIA6, CQUAD4, CQUAD8 ---
C
      ALLOCATE( J_SHEL(10,N_SHEL) )
      J_SHEL(:,:) = 0
C
      I_SHEL = 0
C
C     --- CTETRA, CPENTA, CHEXA ---
C
      ALLOCATE( J_SOL(22,N_SOL) )
      J_SOL(:,:) = 0
C
      I_SOL = 0
C
C     --- CROD ---
C
      ALLOCATE( J_ROD(4,N_ROD) )
      J_ROD(:,:) = 0
C
      I_ROD = 0
C
C     --- CBAR ---
C
      ALLOCATE( J_BAR(4,N_BAR) )
      ALLOCATE( R_BAR(3,N_BAR) )
      J_BAR(:,:) = 0
      R_BAR(:,:) = 0.
C
      I_BAR = 0
C
C     --- CELAS1, CELAS2 ---
C
      ALLOCATE( J_ELS(6,N_ELS) )
      ALLOCATE( R_ELS(3,N_ELS) )
      J_ELS(:,:) = 0
      R_ELS(:,:) = 0.
C
      I_ELS = 0
C
C     --- PSHELL ---
C
      ALLOCATE( J_PSHEL(2,N_PSHEL) )
      ALLOCATE( R_PSHEL(N_PSHEL) )
      J_PSHEL(:,:) = 0
      R_PSHEL(:) = 0.
C
      I_PSHEL = 0
C
C     --- PSOLID ---
C
      ALLOCATE( J_PSOL(3,N_PSOL) )
      J_PSOL(:,:) = 0
C
      I_PSOL = 0
C
C     --- PROD ---
C
      ALLOCATE( J_PROD(2,N_PROD) )
      ALLOCATE( R_PROD(N_PROD) )
      J_PROD(:,:) = 0
      R_PROD(:) = 0.
C
      I_PROD = 0
C
C     --- PBAR, PBARL ---
C
      ALLOCATE( J_PBAR(3,N_PBAR) )
      ALLOCATE( R_PBAR(6,N_PBAR) )
      J_PBAR(:,:) = 0
      R_PBAR(:,:) = 0.
C
      I_PBAR = 0
C
C     --- PELAS ---
C
      ALLOCATE( J_PELS(N_PELS) )
      ALLOCATE( R_PELS(3,N_PELS) )
      J_PELS(:) = 0
      R_PELS(:,:) = 0.
C
      I_PELS = 0
C
C     ---- MAT1, MAT9 ---
C
      NMAT = N_MAT
C
      KK(11) = NMAT
C
      ALLOCATE( MAT(2,NMAT) )
      ALLOCATE( AMAT(33,NMAT) )
      MAT(:,:) = 0
      AMAT(:,:) = 0.
C
      ALLOCATE( IMATR(M_MAT) )
      IMATR(:) = 0
C
      I_MAT = 0
C
C     ---- MATT1 ---
C
      ALLOCATE( J_MATT1(6,N_MATT1) )
      J_MATT1(:,:) = 0
C
      I_MATT1 = 0
C
C     ---- TABLEM1 ---
C
      NITM1 = N_TBM1
      NTBM1 = N2_TBM1
C
      KK(72) = NITM1
      KK(73) = NTBM1
C
      ALLOCATE( ITM1(NITM1) )
      ALLOCATE( TBM1(2,NTBM1) )
      ITM1(:) = 0
      TBM1(:,:) = 0.
C
      ALLOCATE( ITBM1R(M_TBM1) )
      ITBM1R(:) = 0
C
      I_TBM1 = 0
      I2_TBM1 = 0
C
C     ---- MATS1 ---
C
      ALLOCATE( J_MATS1(2,N_MATS1) )
      ALLOCATE( R_MATS1(3,N_MATS1) )
      J_MATS1(:,:) = 0
      R_MATS1(:,:) = 0.
C
      I_MATS1 = 0
C
C     --- SPCD ---
C
      ALLOCATE( J_SPCD(8,N_SPCD) )
      ALLOCATE( R_SPCD(N_SPCD) )
      J_SPCD(:,:) = 0
      R_SPCD(:) = 0.
C
      I_SPCD = 0
C
C     ---- LOAD ---
C
      NLOAD = N_LD
      NNLOAD = N2_LD
C
      KK(40) = NLOAD
      KK(41) = NNLOAD
C
      ALLOCATE( LOAD(2,NLOAD) )
      ALLOCATE( SLOD(NLOAD) )
      ALLOCATE( SILD(NNLOAD) )
      ALLOCATE( LILD(NNLOAD) )
      LOAD(:,:) = 0
      SLOD(:) = 0.
      SILD(:) = 0.
      LILD(:) = 0
C
      I_LD = 0
      I2_LD = 0
C
C     --- FORCE, MOMENT ---
C
      ALLOCATE( J_FRC(3,N_FRC) )
      ALLOCATE( R_FRC(6,N_FRC) )
      J_FRC(:,:) = 0
      R_FRC(:,:) = 0.
C
      I_FRC = 0
C
C     --- PLOAD4, PLOAD2 ---
C
      ALLOCATE( J_PL4(6,N_PL4+N_PL) )
      ALLOCATE( R_PL4(4,N_PL4+N_PL) )
      J_PL4(:,:) = 0
      R_PL4(:,:) = 0.
C
      I_PL4 = 0
C
C     --- PLOAD ---
C
      ALLOCATE( J_PL(5,N_PL) )
      ALLOCATE( R_PL(N_PL) )
      J_PL(:,:) = 0
      R_PL(:) = 0.
C
      I_PL = 0
C
C     --- PLOAD1 ---
C
      ALLOCATE( J_PL1(3,N_PL1) )
      ALLOCATE( R_PL1(N_PL1) )
      J_PL1(:,:) = 0
      R_PL1(:) = 0.
C
      I_PL1 = 0
C
C     ---- GRAV ---
C
      NIGRV = N_GRV
C
      KK(60) = NIGRV
C
      ALLOCATE( IGRV(NIGRV) )
      ALLOCATE( GRAV(3,NIGRV) )
      IGRV(:) = 0
      GRAV(:,:) = 0.
C
      I_GRV = 0
C
C     ---- RFORCE ---
C
      NIRFC = N_RFC
C
      KK(61) = NIRFC
C
      ALLOCATE( IRFC(2,NIRFC) )
      ALLOCATE( RFRC(3,NIRFC) )
      IRFC(:,:) = 0
      RFRC(:,:) = 0.
C
      I_RFC = 0
C
C     --- TEMPD ---
C
      ALLOCATE( J_TMPD(N_TMPD) )
      ALLOCATE( R_TMPD(N_TMPD) )
      J_TMPD(:) = 0
      R_TMPD(:) = 0.
C
      I_TMPD = 0
C
C     --- TEMP ---
C
      ALLOCATE( J_TMP(2,N_TMP) )
      ALLOCATE( R_TMP(N_TMP) )
      J_TMP(:,:) = 0
      R_TMP(:) = 0.
C
      I_TMP = 0
C
C     --- TEMPP1 ---
C
      ALLOCATE( J_TMP1(4,N_TMP1) )
      ALLOCATE( R_TMP1(2,N_TMP1) )
      J_TMP1(:,:) = 0
      R_TMP1(:,:) = 0.
C
      I_TMP1 = 0
C
C     ---- SPCADD ---
C
      NISPA = N_SPCAD
      NNSPA = N2_SPCAD
C
      KK(46) = NISPA
      KK(47) = NNSPA
C
      ALLOCATE( ISPA(2,NISPA) )
      ALLOCATE( NSPA(NNSPA) )
      ISPA(:,:) = 0
      NSPA(:) = 0
C
      I_SPCAD = 0
      I2_SPCAD = 0
C
C     --- SPC1 ---
C
      ALLOCATE( J_SPC1(9,N_SPC1) )
      J_SPC1(:,:) = 0
C
      I_SPC1 = 0
C
C     --- SPC ---
C
      ALLOCATE( J_SPC(8,N_SPC) )
      ALLOCATE( R_SPC(N_SPC) )
      J_SPC(:,:) = 0
      R_SPC(:) = 0.
C
      I_SPC = 0
C
C     ---- MPCADD ---
C
      NIMPA = N_MPCAD
      NNMPA = N2_MPCAD
C
      KK(50) = NIMPA
      KK(51) = NNMPA
C
      ALLOCATE( IMPA(2,NIMPA) )
      ALLOCATE( NMPA(NNMPA) )
      IMPA(:,:) = 0
      NMPA(:) = 0
C
      I_MPCAD = 0
      I2_MPCAD = 0
C
C     --- MPC ---
C
      ALLOCATE( J_MPC(3,N_MPC) )
      ALLOCATE( J2_MPC(2,N2_MPC) )
      ALLOCATE( R2_MPC(N2_MPC) )
      J_MPC(:,:) = 0
      J2_MPC(:,:) = 0
      R2_MPC(:) = 0.
C
      I_MPC = 0
      I2_MPC = 0
C
C     ---- EIGRL ---
C
      NNEIG = N_EIG
C
      KK(55) = NNEIG
C
      ALLOCATE( NEIG(2,NNEIG) )
      NEIG(:,:) = 0
C
      I_EIG = 0
C
C     ---- FREQ1 ---
C
      NIFR1 = N_FRQ1
C
      KK(65) = NIFR1
C
      ALLOCATE( IFR1(2,NIFR1) )
      ALLOCATE( FRQ1(2,NIFR1) )
      IFR1(:,:) = 0
      FRQ1(:,:) = 0.
C
      I_FRQ1 = 0
C
C     ---- DLOAD ---
C
      NIDLD = N_DLD
      NSIDL = N2_DLD
C
      KK(66) = NIDLD
      KK(67) = NSIDL
C
      ALLOCATE( IDLD(2,NIDLD) )
      ALLOCATE( SDLD(NIDLD) )
      ALLOCATE( SIDL(NSIDL) )
      ALLOCATE( LIDL(NSIDL) )
      IDLD(:,:) = 0
      SDLD(:) = 0.
      SIDL(:) = 0.
      LIDL(:) = 0
C
      I_DLD = 0
      I2_DLD = 0
C
C     ---- RLOAD1 ---
C
      NIRL1 = N_RLD1
C
      KK(68) = NIRL1
C
      ALLOCATE( IRL1(7,NIRL1) )
      IRL1(:,:) = 0
C
      I_RLD1 = 0
C
C     ---- LSEQ ---
C
      NLSEQ = N_LSEQ
C
      KK(69) = NLSEQ
C
      ALLOCATE( LSEQ(4,NLSEQ) )
      LSEQ(:,:) = 0
C
      I_LSEQ = 0
C
C     ---- TABLED1 ---
C
      NITD1 = N_TBD1
      NTBD1 = N2_TBD1
C
      KK(70) = NITD1
      KK(71) = NTBD1
C
      ALLOCATE( ITD1(NITD1) )
      ALLOCATE( TBD1(2,NTBD1) )
      ITD1(:) = 0
      TBD1(:,:) = 0.
C
      ALLOCATE( ITBD1R(M_TBD1) )
      ITBD1R(:) = 0
C
      I_TBD1 = 0
      I2_TBD1 = 0
C
C     ---- NLPARM, TSTEPNL ---
C
      NNLP = N_NLP
C
      KK(76) = NNLP
C
      ALLOCATE( NLP(4,NNLP) )
      ALLOCATE( RNLP(2,NNLP) )
      NLP(:,:) = 0
      RNLP(:,:) = 0.
C
      I_NLP = 0
C
C     ---- TSTEP ---
C
      NISTP = N_TSTP
      NNSTP = N2_TSTP
C
      KK(78) = NISTP
      KK(79) = NNSTP
C
      ALLOCATE( ISTP(2,NISTP) )
      ALLOCATE( NSTP(2,NNSTP) )
      ALLOCATE( DELT(NNSTP) )
      ISTP(:,:) = 0
      NSTP(:,:) = 0
      DELT(:) = 0.
C
      I_TSTP = 0
      I2_TSTP = 0
C
C     ---- TLOAD1 ---
C
      NITL1 = N_TLD1
C
      KK(77) = NITL1
C
      ALLOCATE( ITL1(4,NITL1) )
      ITL1(:,:) = 0
C
      I_TLD1 = 0
C
C     ---- BCTADD ---
C
      NICPA = N_BCTAD
      NNCPA = N2_BCTAD
C
      KK(88) = NICPA
      KK(89) = NNCPA
C
      ALLOCATE( ICPA(2,NICPA) )
      ALLOCATE( NCPA(NNCPA) )
      ICPA(:,:) = 0
      NCPA(:) = 0
C
      I_BCTAD = 0
      I2_BCTAD = 0
C
C     --- BCTSET ---
C
      ALLOCATE( J_BCTS(3,N_BCTS) )
      J_BCTS(:,:) = 0
C
      I_BCTS = 0
C
C     --- BCTPARA ---
C
      ALLOCATE( J_BCTP(N_BCTP) )
      ALLOCATE( R_BCTP(2,N_BCTP) )
      J_BCTP(:) = 0
      R_BCTP(:,:) = 0.
C
      I_BCTP = 0
C
C     --- BSURFS ---
C
      ALLOCATE( J_BSRF(5,N_BSRF) )
      J_BSRF(:,:) = 0
C
      I_BSRF = 0
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == 'ENDDATA ' ) THEN
          EXIT
C
        ELSEIF( CHAR(1:4) == 'REST' ) THEN
          CALL RD_REST( RTIM, I_RST, CHAR )
C
        ELSEIF( CHAR(1:5) == 'PARAM' ) THEN
          CALL RD_PARAM( KK, RR, CHAR )
C
        ELSEIF( CHAR(1:8) == 'CORD2C  ' ) THEN
          I_CORD = I_CORD + 1
          CALL RD_CORD2C( ICRD(I_CORD), CORD(1,I_CORD), ICRDR, I_CORD,
     &                    CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'CORD2R  ' ) THEN
          I_CORD = I_CORD + 1
          CALL RD_CORD2R( ICRD(I_CORD), CORD(1,I_CORD), ICRDR, I_CORD,
     &                    CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'GRID    ' ) THEN
          I_GRID = I_GRID + 1
          CALL RD_GRID( INDG(I_GRID), IGCD(I_GRID), GRID(1,I_GRID),
     &                  INDGR, I_GRID, CHAR, ITI, 0 )
C
        ELSEIF( CHAR(1:8) == 'GRID*   ' ) THEN
          I_GRID = I_GRID + 1
          CALL RD_GRID( INDG(I_GRID), IGCD(I_GRID), GRID(1,I_GRID),
     &                  INDGR, I_GRID, CHAR, ITI, 1 )
C
        ELSEIF( CHAR(1:8) == 'CTRIA3  ' ) THEN
          I_SHEL = I_SHEL + 1
          CALL RD_SHELL( J_SHEL(1,I_SHEL), 3, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'CTRIA6  ' ) THEN
          I_SHEL = I_SHEL + 1
          CALL RD_SHELL( J_SHEL(1,I_SHEL), 6, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'CQUAD4  ' ) THEN
          I_SHEL = I_SHEL + 1
          CALL RD_SHELL( J_SHEL(1,I_SHEL), 4, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'CQUAD8  ' ) THEN
          I_SHEL = I_SHEL + 1
          CALL RD_SHELL( J_SHEL(1,I_SHEL), 8, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'CTETRA  ' .OR. 
     &          CHAR(1:8) == 'CPENTA  ' .OR.
     &          CHAR(1:8) == 'CHEXA   ' ) THEN
          I_SOL = I_SOL + 1
          CALL RD_SOLID( J_SOL(1,I_SOL), CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'CROD    ' ) THEN
          I_ROD = I_ROD + 1
          READ(CHAR,'(BN,8X,4I8)') J_ROD(:,I_ROD)
C
        ELSEIF( CHAR(1:8) == 'CBAR    ' ) THEN
          I_BAR = I_BAR + 1
          READ(CHAR,'(BN,8X,4I8,3F8.0)') J_BAR(:,I_BAR), R_BAR(:,I_BAR)
C
        ELSEIF( CHAR(1:8) == 'CELAS1  ' ) THEN
          I_ELS = I_ELS + 1
          READ(CHAR,'(BN,8X,6I8)') J_ELS(:,I_ELS)
C
        ELSEIF( CHAR(1:8) == 'CELAS2  ' ) THEN
          I_ELS = I_ELS + 1
          READ(CHAR,'(BN,8X,I8,F8.0,4I8,2F8.0)') 
     &      J_ELS(1,I_ELS), R_ELS(1,I_ELS), J_ELS(3:6,I_ELS), 
     &      R_ELS(2:3,I_ELS)
C
        ELSEIF( CHAR(1:8) == 'PSHELL  ' ) THEN
          I_PSHEL = I_PSHEL + 1
          READ(CHAR,'(BN,8X,2I8,F8.0)') J_PSHEL(:,I_PSHEL),
     &                                  R_PSHEL(I_PSHEL)
C
        ELSEIF( CHAR(1:8) == 'PSOLID  ' ) THEN
          I_PSOL = I_PSOL + 1
          READ(CHAR,'(BN,8X,3I8)') J_PSOL(:,I_PSOL)
C
        ELSEIF( CHAR(1:8) == 'PROD    ' ) THEN
          I_PROD = I_PROD + 1
          READ(CHAR,'(BN,8X,2I8,F8.0)') J_PROD(:,I_PROD),
     &                                  R_PROD(I_PROD)
C
        ELSEIF( CHAR(1:8) == 'PBAR    ' ) THEN
          I_PBAR = I_PBAR + 1
          CALL RD_PBAR( J_PBAR(1,I_PBAR), R_PBAR(1,I_PBAR), CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'PBARL   ' ) THEN
          I_PBAR = I_PBAR + 1
          CALL RD_PBARL( J_PBAR(1,I_PBAR), R_PBAR(1,I_PBAR), CHAR, ITI,
     &                   ITO )
C
        ELSEIF( CHAR(1:8) == 'PELAS   ' ) THEN
          I_PELS = I_PELS + 1
          READ(CHAR,'(BN,8X,I8,3F8.0)') 
     &      J_PELS(I_PELS), R_PELS(:,I_PELS)
          IF( CHAR(41:48) /= BLK ) THEN
            I_PELS = I_PELS + 1
            READ(CHAR,'(BN,40X,I8,3F8.0)') 
     &        J_PELS(I_PELS), R_PELS(:,I_PELS)
          ENDIF
C
        ELSEIF( CHAR(1:8) == 'MAT1    ' ) THEN
          I_MAT = I_MAT + 1
          CALL RD_MAT1( AMAT(1,I_MAT), IMATR, I_MAT, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'MAT9    ' ) THEN
          I_MAT = I_MAT + 1
          CALL RD_MAT9( MAT(1,I_MAT), AMAT(1,I_MAT), IMATR, I_MAT, CHAR,
     &                  ITI )
C
        ELSEIF( CHAR(1:8) == 'MATT1   ' ) THEN
          I_MATT1 = I_MATT1 + 1
          CALL RD_MATT1( J_MATT1(1,I_MATT1), CHAR )
C
        ELSEIF( CHAR(1:8) == 'TABLEM1 ' ) THEN
          I_TBM1 = I_TBM1 + 1
          CALL RD_TABLEM1( ITM1(I_TBM1), TBM1, ITBM1R, I_TBM1, I2_TBM1,
     &                     CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'MATS1   ' ) THEN
          I_MATS1 = I_MATS1 + 1
          CALL RD_MATS1( J_MATS1(1,I_MATS1), R_MATS1(1,I_MATS1), CHAR,
     &                   ITO )
C
        ELSEIF( CHAR(1:8) == 'SPCD    ' ) THEN
          CALL RD_SPC( J_SPCD, R_SPCD, I_SPCD, CHAR )
C
        ELSEIF( CHAR(1:8) == 'LOAD    ' ) THEN
          I_LD = I_LD + 1
          CALL RD_LOAD( LOAD(1,I_LD), SLOD(I_LD), SILD, LILD, I2_LD,
     &                  CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'FORCE   ' .OR. 
     &          CHAR(1:8) == 'MOMENT  ' ) THEN
          I_FRC = I_FRC + 1
          CALL RD_FORCE( J_FRC(1,I_FRC), R_FRC(1,I_FRC), CHAR )
C
        ELSEIF( CHAR(1:8) == 'PLOAD4  ' ) THEN
          I_PL4 = I_PL4 + 1
          CALL RD_PLOAD4( J_PL4(1,I_PL4), R_PL4(1,I_PL4), CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'PLOAD2  ' ) THEN
          CALL RD_PLOAD2( J_PL4, R_PL4, I_PL4, CHAR )
C
        ELSEIF( CHAR(1:8) == 'PLOAD   ' ) THEN
          I_PL = I_PL + 1
          CALL RD_PLOAD( J_PL(1,I_PL), R_PL(I_PL), CHAR )
C
        ELSEIF( CHAR(1:8) == 'PLOAD1  ' ) THEN
          I_PL1 = I_PL1 + 1
          CALL RD_PLOAD1( J_PL1(1,I_PL1), R_PL1(I_PL1), CHAR, ITO )
C
        ELSEIF( CHAR(1:8) == 'GRAV    ' ) THEN
          I_GRV = I_GRV + 1
          CALL RD_GRAV( IGRV(I_GRV), GRAV(1,I_GRV), CHAR )
C
        ELSEIF( CHAR(1:8) == 'RFORCE  ' ) THEN
          I_RFC = I_RFC + 1
          CALL RD_RFORCE( IRFC(1,I_RFC), RFRC(1,I_RFC), CHAR )
C
        ELSEIF( CHAR(1:8) == 'TEMPD   ' ) THEN
          CALL RD_TEMPD( J_TMPD, R_TMPD, I_TMPD, CHAR )
C
        ELSEIF( CHAR(1:8) == 'TEMP    ' ) THEN
          CALL RD_TEMP( J_TMP, R_TMP, I_TMP, CHAR )
C
        ELSEIF( CHAR(1:8) == 'TEMPP1  ' ) THEN
          CALL RD_TEMPP1( J_TMP1, R_TMP1, I_TMP1, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'SPCADD  ' ) THEN
          I_SPCAD = I_SPCAD + 1
          CALL RD_SPCADD( ISPA(1,I_SPCAD), NSPA, I2_SPCAD, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'SPC1    ' ) THEN
          CALL RD_SPC1( J_SPC1, I_SPC1, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'SPC     ' ) THEN
          CALL RD_SPC( J_SPC, R_SPC, I_SPC, CHAR )
C
        ELSEIF( CHAR(1:8) == 'MPCADD  ' ) THEN
          I_MPCAD = I_MPCAD + 1
          CALL RD_SPCADD( IMPA(1,I_MPCAD), NMPA, I2_MPCAD, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'MPC     ' ) THEN
          I_MPC = I_MPC + 1
          CALL RD_MPC( J_MPC(1,I_MPC), J2_MPC, R2_MPC, I2_MPC, CHAR, 
     &                 ITI )
C
        ELSEIF( CHAR(1:8) == 'EIGRL   ' ) THEN
          I_EIG = I_EIG + 1
          READ(CHAR,'(BN,8X,I8,16X,I8)') NEIG(:,I_EIG)
C
        ELSEIF( CHAR(1:8) == 'FREQ1   ' ) THEN
          I_FRQ1 = I_FRQ1 + 1
          READ(CHAR,'(BN,8X,I8,2F8.0,I8)') 
     &      IFR1(1,I_FRQ1), FRQ1(:,I_FRQ1),IFR1(2,I_FRQ1)
C
        ELSEIF( CHAR(1:8) == 'DLOAD   ' ) THEN
          I_DLD = I_DLD + 1
          CALL RD_LOAD( IDLD(1,I_DLD), SDLD(I_DLD), SIDL, LIDL, I2_DLD,
     &                  CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'RLOAD1  ' ) THEN
          I_RLD1 = I_RLD1 + 1
          CALL RD_RLOAD1( IRL1(1,I_RLD1), CHAR )
C
        ELSEIF( CHAR(1:8) == 'LSEQ    ' ) THEN
          I_LSEQ = I_LSEQ + 1
          READ(CHAR,'(BN,8X,4I8)') LSEQ(:,I_LSEQ)
C
        ELSEIF( CHAR(1:8) == 'TABLED1 ' .OR.
     &          CHAR(1:8) == 'TABLED2 ' ) THEN
          I_TBD1 = I_TBD1 + 1
          CALL RD_TABLEM1( ITD1(I_TBD1), TBD1, ITBD1R, I_TBD1, I2_TBD1,
     &                     CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'NLPARM  ' ) THEN
          I_NLP = I_NLP + 1
          CALL RD_NLPARM( NLP(1,I_NLP), RNLP(1,I_NLP), CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'TSTEPNL ' ) THEN
          I_NLP = I_NLP + 1
          CALL RD_TSTEPNL( NLP(1,I_NLP), RNLP(1,I_NLP), CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'TSTEP   ' ) THEN
          I_TSTP = I_TSTP + 1
          CALL RD_TSTEP( ISTP(1,I_TSTP), NSTP, DELT, I2_TSTP, CHAR, ITI)
C
        ELSEIF( CHAR(1:8) == 'TLOAD1  ' ) THEN
          I_TLD1 = I_TLD1 + 1
          CALL RD_TLOAD1( ITL1(1,I_TLD1), CHAR )
C
        ELSEIF( CHAR(1:8) == 'BCTADD  ' ) THEN
          I_BCTAD = I_BCTAD + 1
          CALL RD_SPCADD( ICPA(1,I_BCTAD), NCPA, I2_BCTAD, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'BCTSET  ' ) THEN
          CALL RD_BCTSET( J_BCTS, I_BCTS, CHAR, ITI )
C
        ELSEIF( CHAR(1:8) == 'BCTPARA ' ) THEN
          I_BCTP = I_BCTP + 1
          READ(CHAR,'(BN,8X,I8,24X,F8.0,8X,F8.0)')
     &    J_BCTP(I_BCTP), R_BCTP(:,I_BCTP)
C
        ELSEIF( CHAR(1:8) == 'BSURFS  ' ) THEN
          CALL RD_BSURFS( J_BSRF, I_BSRF, CHAR, ITI )
C
        ELSEIF( CHAR(1:4) == 'FRIC' ) THEN
          CALL RD_FRIC( KK, RR, CHAR )
C
        ELSEIF( CHAR(1:7) == 'CONTACT' ) THEN
          CALL RD_CONTACT( RR, CHAR )
C
        ENDIF
C
      ENDDO
C
      END