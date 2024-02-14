      SUBROUTINE VF_IIMATE(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIMATE:物性値等データ(MATE)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')

CD    -- MATE W-LEVELを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'W-LEVEL') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(WVLVL,TEXT(IS(3):IE(3)))

CD    -- MATE DENSITYを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'DENSITY') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(RHO0,TEXT(IS(3):IE(3)))
        IF (RHO0.LT.ZERO) CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')

CD    -- MATE K-VISCを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'K-VISC') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(ANU0,TEXT(IS(3):IE(3)))
        IF (ANU0.LT.0.0D0) CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')

CD    -- MATE GRAVITYを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'GRAVITY') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(GRZ0,TEXT(IS(3):IE(3)))
        IF (GRZ0.LT.0.0D0) CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')

CD    -- MATE S-HEATを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'S-HEAT') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(TCP0,TEXT(IS(3):IE(3)))
        IF (TCP0.LT.ZERO) CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')

CD    -- MATE T-CONDを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'T-COND') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(TCN0,TEXT(IS(3):IE(3)))
        IF (TCN0.LT.ZERO) CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')

CD    -- MATE T-DENSを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'T-DENS') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOR(TDT0,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOR(TDR0,TEXT(IS(4):IE(4)))

CD    -- MATE DIFFUSを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'DIFFUS') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOI(LC,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOR(SC,TEXT(IS(4):IE(4)))
        IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                  CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')
        IF (SC.LT.ZERO) CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')
        CDF0(LC)=SC

CD    -- MATE C-DENSを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'C-DENS') THEN
        IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
        CALL VF_ZSTOI(LC,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOR(SC,TEXT(IS(4):IE(4)))
        CALL VF_ZSTOR(SR,TEXT(IS(5):IE(5)))
        IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                  CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')
        CDC0(LC)=SC
        CDR0(LC)=SR

CD    -- MATE I.C.を解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'I.C.') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
C       * MATE I.C. V
        IF     (TEXT(IS(3):IE(3)).EQ.'V') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
          CALL VF_ZSTOR(UINI,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(VINI,TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(WINI,TEXT(IS(6):IE(6)))
C       * MATE I.C. KE
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'KE') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
          CALL VF_ZSTOR(AKINIK,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(AKINIE,TEXT(IS(5):IE(5)))
          IF (AKINIK.LT.ZERO)
     &                 CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')
          IF (AKINIE.LT.ZERO)
     &                 CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')
C       * MATE I.C. T
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'T') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
          CALL VF_ZSTOR(TINI,TEXT(IS(4):IE(4)))
C       * MATE I.C. C
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'C') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMATE','SYNTAX ERROR.')
          CALL VF_ZSTOI(LC,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(SC,TEXT(IS(5):IE(5)))
          IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                 CALL VF_A2ERR('VF_IIMATE','INVALID VALUE.')
          CINI(LC)=SC
C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IIMATE','UNKNOWN WORD.')
        ENDIF

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIMATE','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
