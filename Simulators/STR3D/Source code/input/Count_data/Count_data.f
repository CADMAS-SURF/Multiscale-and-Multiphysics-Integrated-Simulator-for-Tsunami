      SUBROUTINE COUNT_DATA( ITI )
C
      USE INPUT_WORK
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
C
      N_SUB    = 0
      N_RST    = 0
      N_CORD   = 0
      M_CORD   = 0
      N_GRID   = 0
      M_GRID   = 0
      N_SHEL   = 0
      N_SOL    = 0
      N_ROD    = 0
      N_BAR    = 0
      N_ELS    = 0
      M_ELEM   = 0
      N_PSHEL  = 0
      N_PSOL   = 0
      N_PROD   = 0
      N_PBAR   = 0
      N_PELS   = 0
      N_MAT    = 0
      M_MAT    = 0
      N_MATT1  = 0
      N_TBM1   = 0
      N2_TBM1  = 0
      M_TBM1   = 0
      N_MATS1  = 0
      N_SPCD   = 0
      N_LD     = 0
      N2_LD    = 0
      N_FRC    = 0
      N_PL4    = 0
      N_PL     = 0
      N_PL1    = 0
      N_GRV    = 0
      N_RFC    = 0
      N_TMPD   = 0
      N_TMP    = 0
      N_TMP1   = 0
      N_SPCAD  = 0
      N2_SPCAD = 0
      N_SPC1   = 0
      N_SPC    = 0
      N_MPCAD  = 0
      N2_MPCAD = 0
      N_MPC    = 0
      N2_MPC   = 0
      N_EIG    = 0
      N_FRQ1   = 0
      N_DLD    = 0
      N2_DLD   = 0
      N_RLD1   = 0
      N_LSEQ   = 0
      N_TBD1   = 0
      N2_TBD1  = 0
      M_TBD1   = 0
      N_NLP    = 0
      N_TSTP   = 0
      N2_TSTP  = 0
      N_TLD1   = 0
      N_BCTAD  = 0
      N2_BCTAD = 0
      N_BCTS   = 0
      N_BCTP   = 0
      N_BSRF   = 0
      M_BSRF   = 0
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:7) == 'ENDDATA' ) THEN
          EXIT
        ELSEIF( CHAR(1:7) == 'SUBCASE' ) THEN
          N_SUB = N_SUB + 1
        ELSEIF( CHAR(1:9) == 'REST,TIME' ) THEN
          N_RST = N_RST + 1
        ELSEIF( CHAR(1:8) == 'CORD2C  ' .OR. 
     &          CHAR(1:8) == 'CORD2R  ' ) THEN
          N_CORD = N_CORD + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_CORD ) M_CORD = NO
        ELSEIF( CHAR(1:8) == 'GRID    ' ) THEN
          N_GRID = N_GRID + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_GRID ) M_GRID = NO
        ELSEIF( CHAR(1:8) == 'GRID*   ' ) THEN
          N_GRID = N_GRID + 1
          READ(CHAR,'(BN,8X,I16)') NO
          IF( NO > M_GRID ) M_GRID = NO
        ELSEIF( CHAR(1:8) == 'CTRIA3  ' .OR. 
     &          CHAR(1:8) == 'CTRIA6  ' .OR. 
     &          CHAR(1:8) == 'CQUAD4  ' .OR. 
     &          CHAR(1:8) == 'CQUAD8  ' ) THEN
          N_SHEL = N_SHEL + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_ELEM ) M_ELEM = NO
        ELSEIF( CHAR(1:8) == 'CTETRA  ' .OR. 
     &          CHAR(1:8) == 'CPENTA  ' .OR.
     &          CHAR(1:8) == 'CHEXA   ' ) THEN
          N_SOL = N_SOL + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_ELEM ) M_ELEM = NO
        ELSEIF( CHAR(1:8) == 'CROD    ' ) THEN
          N_ROD = N_ROD + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_ELEM ) M_ELEM = NO
        ELSEIF( CHAR(1:8) == 'CBAR    ' ) THEN
          N_BAR = N_BAR + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_ELEM ) M_ELEM = NO
        ELSEIF( CHAR(1:8) == 'CELAS1  ' .OR. 
     &          CHAR(1:8) == 'CELAS2  ' ) THEN
          N_ELS = N_ELS + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_ELEM ) M_ELEM = NO
        ELSEIF( CHAR(1:8) == 'PSHELL  ' ) THEN
          N_PSHEL = N_PSHEL + 1
        ELSEIF( CHAR(1:8) == 'PSOLID  ' ) THEN
          N_PSOL = N_PSOL + 1
        ELSEIF( CHAR(1:8) == 'PROD    ' ) THEN
          N_PROD = N_PROD + 1
        ELSEIF( CHAR(1:8) == 'PBAR    ' .OR. 
     &          CHAR(1:8) == 'PBARL   ' ) THEN
          N_PBAR = N_PBAR + 1
        ELSEIF( CHAR(1:8) == 'PELAS   ' ) THEN
          N_PELS = N_PELS + 1
          IF( CHAR(41:48) /= BLK ) N_PELS = N_PELS + 1
        ELSEIF( CHAR(1:8) == 'MAT1    ' .OR. 
     &          CHAR(1:8) == 'MAT9    ' ) THEN
          N_MAT = N_MAT + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_MAT ) M_MAT = NO
        ELSEIF( CHAR(1:8) == 'MATT1   ' ) THEN
          N_MATT1 = N_MATT1 + 1
        ELSEIF( CHAR(1:8) == 'TABLEM1 ' ) THEN
          N_TBM1 = N_TBM1 + 1
          CALL CT_TABLEM1( N2_TBM1, ITI )
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_TBM1 ) M_TBM1 = NO
        ELSEIF( CHAR(1:8) == 'MATS1   ' ) THEN
          N_MATS1 = N_MATS1 + 1
        ELSEIF( CHAR(1:8) == 'SPCD    ' ) THEN
          CALL CT_SPC( N_SPCD, CHAR )
        ELSEIF( CHAR(1:8) == 'LOAD    ' ) THEN
          N_LD = N_LD + 1
          CALL CT_LOAD( N2_LD, CHAR, ITI )
        ELSEIF( CHAR(1:8) == 'FORCE   ' .OR. 
     &          CHAR(1:8) == 'MOMENT  ' ) THEN
          N_FRC = N_FRC + 1
        ELSEIF( CHAR(1:8) == 'PLOAD4  ' ) THEN
          N_PL4 = N_PL4 + 1
        ELSEIF( CHAR(1:8) == 'PLOAD2  ' ) THEN
          CALL CT_PLOAD2( N_PL4, CHAR )
        ELSEIF( CHAR(1:8) == 'PLOAD   ' ) THEN
          N_PL  = N_PL  + 1
        ELSEIF( CHAR(1:8) == 'PLOAD1  ' ) THEN
          N_PL1 = N_PL1 + 1
        ELSEIF( CHAR(1:8) == 'GRAV    ' ) THEN
          N_GRV = N_GRV + 1
        ELSEIF( CHAR(1:8) == 'RFORCE  ' ) THEN
          N_RFC = N_RFC + 1
        ELSEIF( CHAR(1:8) == 'TEMPD   ' ) THEN
          CALL CT_TEMPD( N_TMPD, CHAR )
        ELSEIF( CHAR(1:8) == 'TEMP    ' ) THEN
          CALL CT_TEMP( N_TMP, CHAR )
        ELSEIF( CHAR(1:8) == 'TEMPP1  ' ) THEN
          CALL CT_TEMPP1( N_TMP1, ITI )
        ELSEIF( CHAR(1:8) == 'SPCADD  ' ) THEN
          N_SPCAD = N_SPCAD + 1
          CALL CT_SPCADD( N2_SPCAD, CHAR, ITI )
        ELSEIF( CHAR(1:8) == 'SPC1    ' ) THEN
          CALL CT_SPC1( N_SPC1, CHAR, ITI )
        ELSEIF( CHAR(1:8) == 'SPC     ' ) THEN
          CALL CT_SPC( N_SPC, CHAR )
        ELSEIF( CHAR(1:8) == 'MPCADD  ' ) THEN
          N_MPCAD = N_MPCAD + 1
          CALL CT_SPCADD( N2_MPCAD, CHAR, ITI )
        ELSEIF( CHAR(1:8) == 'MPC     ' ) THEN
          N_MPC = N_MPC + 1
          CALL CT_MPC( N2_MPC, ITI )
        ELSEIF( CHAR(1:8) == 'EIGRL   ' ) THEN
          N_EIG = N_EIG + 1
        ELSEIF( CHAR(1:8) == 'FREQ1   ' ) THEN
          N_FRQ1 = N_FRQ1 + 1
        ELSEIF( CHAR(1:8) == 'DLOAD   ' ) THEN
          N_DLD = N_DLD + 1
          CALL CT_LOAD( N2_DLD, CHAR, ITI )
        ELSEIF( CHAR(1:8) == 'RLOAD1  ' ) THEN
          N_RLD1 = N_RLD1 + 1
        ELSEIF( CHAR(1:8) == 'LSEQ    ' ) THEN
          N_LSEQ = N_LSEQ + 1
        ELSEIF( CHAR(1:8) == 'TABLED1 ' .OR.
     &          CHAR(1:8) == 'TABLED2 ' ) THEN
          N_TBD1 = N_TBD1 + 1
          CALL CT_TABLEM1( N2_TBD1, ITI )
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_TBD1 ) M_TBD1 = NO
        ELSEIF( CHAR(1:8) == 'NLPARM  ' .OR. 
     &          CHAR(1:8) == 'TSTEPNL ' ) THEN
          N_NLP = N_NLP + 1
        ELSEIF( CHAR(1:8) == 'TSTEP   ' ) THEN
          N_TSTP = N_TSTP + 1
          CALL CT_TSTEP( N2_TSTP, ITI )
        ELSEIF( CHAR(1:8) == 'TLOAD1  ' ) THEN
          N_TLD1 = N_TLD1 + 1
        ELSEIF( CHAR(1:8) == 'BCTADD  ' ) THEN
          N_BCTAD = N_BCTAD + 1
          CALL CT_SPCADD( N2_BCTAD, CHAR, ITI )
        ELSEIF( CHAR(1:8) == 'BCTSET  ' ) THEN
          CALL CT_BCTSET( N_BCTS, ITI )
        ELSEIF( CHAR(1:8) == 'BCTPARA ' ) THEN
          N_BCTP = N_BCTP + 1
        ELSEIF( CHAR(1:8) == 'BSURFS  ' ) THEN
          CALL CT_BSURFS( N_BSRF, ITI )
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > M_BSRF ) M_BSRF = NO
        ENDIF
C
      ENDDO
C
      END