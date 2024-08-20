      SUBROUTINE VF_IIMDL(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIMDL:モデル等データ(MODEL)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
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
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')

CD    -- MODEL WAVE-BCを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'WAVE-BC') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'X-') THEN
          JD=1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'X+') THEN
          JD=2
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y-') THEN
          JD=3
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y+') THEN
          JD=4
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF
        IF (IBCTYP(1,JD).NE.0 .AND. IBCTYP(1,JD).NE.1)
     &              CALL VF_A2ERR('VF_IIMDL','DEFINED DOUBLY.')
        IBCTYP(1,JD)=1
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(4):IE(4)).EQ.'FUNC'  ) THEN
          IF     (TEXT(IS(5):IE(5)).EQ.'STREAM' ) THEN
            IF (NWD.LT.6) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
            CALL VF_ZSTOI(IVAL,TEXT(IS(6):IE(6)))
            IF (IVAL.LT.1 .OR. IVAL.GT.22)
     &                    CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
            IBCTYP(2,JD)=IVAL
          ELSEIF (TEXT(IS(5):IE(5)).EQ.'STK-CND') THEN
            IBCTYP(2,JD)= 0
          ELSEIF (TEXT(IS(5):IE(5)).EQ.'STOKES' ) THEN
            IBCTYP(2,JD)=-2
          ELSEIF (TEXT(IS(5):IE(5)).EQ.'CNOIDAL') THEN
            IBCTYP(2,JD)=-1
          ELSEIF (TEXT(IS(5):IE(5)).EQ.'MATRIX' ) THEN
            IBCTYP(2,JD)=-3
            MTBTYP=1
          ELSEIF (TEXT(IS(5):IE(5)).EQ.'MATRIX2') THEN
            IBCTYP(2,JD)=-4
            MTBTYP2=1
          ELSE
            CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
          ENDIF
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'DEPTH' ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          BCTYP(1,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'HEIGHT') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          BCTYP(2,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'PERIOD') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          BCTYP(3,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'AMPL'  ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          BCTYP(8,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'AREA'  ) THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOI(L1,TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(L2,TEXT(IS(6):IE(6)))
          L1=L1+1
          L2=L2+1
          IF (JD.EQ.1 .OR. JD.EQ.2) THEN
            IF (L1.LT.2 .OR. L2.GT.NUMJ0-1 .OR. L1.GT.L2)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ELSE
            IF (L1.LT.2 .OR. L2.GT.NUMI0-1 .OR. L1.GT.L2)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ENDIF
          IBCTYP(3,JD)=L1
          IBCTYP(4,JD)=L2
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'ANGLE' ) THEN
          IF (NWD.LT.7) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(BCTYP( 9,JD),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(BCTYP(10,JD),TEXT(IS(6):IE(6)))
          CALL VF_ZSTOR(BCTYP(11,JD),TEXT(IS(7):IE(7)))
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF

CD    -- MODEL WAVE-SRCを解釈する --
      ELSEIF   (TEXT(IS(2):IE(2)).EQ.'WAVE-SRC') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'X') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOI(L1,TEXT(IS(4):IE(4)))
          L1=L1+1
          IF (L1.LT.2 .OR. L1.GT.NUMI0-1)
     &                  CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ISCTYP(1)=L1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOI(L1,TEXT(IS(4):IE(4)))
          L1=L1+1
          IF (L1.LT.2 .OR. L1.GT.NUMJ0-1)
     &                  CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ISCTYP(1)=-L1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'FUNC'  ) THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          IF     (TEXT(IS(4):IE(4)).EQ.'STREAM' ) THEN
            IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
            CALL VF_ZSTOI(IVAL,TEXT(IS(5):IE(5)))
            IF (IVAL.LT.1 .OR. IVAL.GT.22)
     &                    CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
            ISCTYP(2)=IVAL
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'STK-CND') THEN
            ISCTYP(2)= 0
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'STOKES' ) THEN
            ISCTYP(2)=-2
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'CNOIDAL') THEN
            ISCTYP(2)=-1
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'MATRIX' ) THEN
            ISCTYP(2)=-3
            MTBTYP=1
          ELSE
            CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
          ENDIF
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'DEPTH' ) THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(4):IE(4)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          SCTYP(1)=VAL
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'HEIGHT') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(4):IE(4)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          SCTYP(2)=VAL
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'PERIOD') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(4):IE(4)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          SCTYP(3)=VAL
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'AMPL'  ) THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(4):IE(4)))
          SCTYP(8)=VAL
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF

CD    -- MODEL OPEN-BCを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'OPEN-BC') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'X-') THEN
          JD=1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'X+') THEN
          JD=2
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y-') THEN
          JD=3
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y+') THEN
          JD=4
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF
        IF (IBCTYP(1,JD).NE.0 .AND. IBCTYP(1,JD).NE.2)
     &              CALL VF_A2ERR('VF_IIMDL','DEFINED DOUBLY.')
        IBCTYP(1,JD)=2
        IBCTYP(3,JD)=0
        IBCTYP(4,JD)=0
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(4):IE(4)).EQ.'FUNC'  ) THEN
          IF     (TEXT(IS(5):IE(5)).EQ.'TYPE1' ) THEN
            IBCTYP(2,JD)= 0
          ELSE
            CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
          ENDIF
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'DEPTH' ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          BCTYP(1,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'PERIOD') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          BCTYP(3,JD)=VAL
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF

CD    -- MODEL DAMPを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'DAMP') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'X-') THEN
          JD=1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'X+') THEN
          JD=2
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y-') THEN
          JD=3
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'Y+') THEN
          JD=4
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF
        IF (IDAMP(JD).EQ.-1) IDAMP(JD)=-2
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(4):IE(4)).EQ.'DEGREE'  ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOI(IVAL,TEXT(IS(5):IE(5)))
          IF (IVAL.LT.0) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IDAMP(JD)=IVAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'PARAM-XY') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          DAMP(1,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'PARAM-Z' ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          DAMP(2,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'WIDTH'   ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          DAMP(3,JD)=VAL
        ELSEIF (TEXT(IS(4):IE(4)).EQ.'DEPTH'   ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(VAL,TEXT(IS(5):IE(5)))
          IF (VAL.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          DAMP(4,JD)=VAL
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF

CD    -- MODEL K-EPSを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'K-EPS') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'LIM'      ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(AKMINK,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(AKMINE,TEXT(IS(5):IE(5)))
          IF (AKMINK.LT.ZERO)
     &                  CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKMINE.LT.ZERO)
     &                  CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'PARAMETER') THEN
          IF (NWD.LT.9) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(AKCMU,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(AKSGK,TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(AKSGE,TEXT(IS(6):IE(6)))
          CALL VF_ZSTOR(AKC1 ,TEXT(IS(7):IE(7)))
          CALL VF_ZSTOR(AKC2 ,TEXT(IS(8):IE(8)))
          CALL VF_ZSTOR(AKC3 ,TEXT(IS(9):IE(9)))
          IF (AKCMU.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKSGK.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKSGE.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKC1.LT.0.0D0) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKC2.LT.0.0D0) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKC3.LT.0.0D0) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'LOG-LAW'  ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(AKK0,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(AKA0,TEXT(IS(5):IE(5)))
          IF (AKK0.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (AKA0.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'PRANDTL'  ) THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOR(AKPR,TEXT(IS(4):IE(4)))
          IF (AKPR.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'SCHMIDT'  ) THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIMDL','SYNTAX ERROR.')
          CALL VF_ZSTOI(LC,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(SC,TEXT(IS(5):IE(5)))
          IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                    CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          IF (SC.LT.ZERO) CALL VF_A2ERR('VF_IIMDL','INVALID VALUE.')
          AKSM(LC)=SC
        ELSE
          CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
        ENDIF

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIMDL','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
