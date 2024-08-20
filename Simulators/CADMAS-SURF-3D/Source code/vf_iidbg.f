      SUBROUTINE VF_IIDBG(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIDBG:デバッグ用データ(DEBUG)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ADBGI.h'
      INCLUDE 'VF_ADBGR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIDBG','SYNTAX ERROR.')

CD    -- DEBUG F-BOXを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'F-BOX') THEN
        IF (NWD.LT.9) CALL VF_A2ERR('VF_IIDBG','SYNTAX ERROR.')
        DO 100 I=1,6
          CALL VF_ZSTOI(IDBGF(I),TEXT(IS(I+2):IE(I+2)))
          IDBGF(I)=IDBGF(I)+1
 100    CONTINUE
        IF ((IDBGF(1).LT.2       ) .OR.
     &      (IDBGF(2).LT.2       ) .OR.
     &      (IDBGF(3).LT.2       ) .OR.
     &      (IDBGF(1).GT.IDBGF(4)) .OR. 
     &      (IDBGF(2).GT.IDBGF(5)) .OR. 
     &      (IDBGF(3).GT.IDBGF(6)) .OR. 
     &      (IDBGF(4).GE.NUMI0   ) .OR. 
     &      (IDBGF(5).GE.NUMJ0   ) .OR. 
     &      (IDBGF(6).GE.NUMK    )     )
     &                CALL VF_A2ERR('VF_IIDBG','INVALID VALUE.')
        CALL VF_ZSTOR(RDBGF,TEXT(IS(9):IE(9)))
        IF ((RDBGF.LT.0.0D0) .OR.
     &      (RDBGF.GT.1.0D0)     )
     &                CALL VF_A2ERR('VF_IIDBG','INVALID VALUE.')

CD    -- DEBUG TD-VELを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'TD-VEL') THEN
        IF (NWD.LT.11) CALL VF_A2ERR('VF_IIDBG','SYNTAX ERROR.')
        DO 200 I=1,6
          CALL VF_ZSTOI(IDBGTD(I),TEXT(IS(I+2):IE(I+2)))
          IDBGTD(I)=IDBGTD(I)+1
 200    CONTINUE
        IF ((IDBGTD(1).LT.2        ) .OR.
     &      (IDBGTD(2).LT.2        ) .OR.
     &      (IDBGTD(3).LT.2        ) .OR.
     &      (IDBGTD(1).GT.IDBGTD(4)) .OR. 
     &      (IDBGTD(2).GT.IDBGTD(5)) .OR. 
     &      (IDBGTD(3).GT.IDBGTD(6)) .OR. 
     &      (IDBGTD(4).GE.NUMI0    ) .OR. 
     &      (IDBGTD(5).GE.NUMJ0    ) .OR. 
     &      (IDBGTD(6).GE.NUMK     )     )
     &                CALL VF_A2ERR('VF_IIDBG','INVALID VALUE.')
        CALL VF_ZSTOR(RDBGTD(1),TEXT(IS( 9):IE( 9)))
        CALL VF_ZSTOR(RDBGTD(2),TEXT(IS(10):IE(10)))
        CALL VF_ZSTOR(RDBGTD(3),TEXT(IS(11):IE(11)))

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIDBG','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
