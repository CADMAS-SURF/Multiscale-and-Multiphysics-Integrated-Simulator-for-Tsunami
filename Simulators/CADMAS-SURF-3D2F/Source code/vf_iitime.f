      SUBROUTINE VF_IITIME(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IITIME:時間制御データ(TIME)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IITIME','SYNTAX ERROR.')

CD    -- TIME CONSTを解釈する --
      IF (TEXT(IS(2):IE(2)).EQ.'CONST') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IITIME','SYNTAX ERROR.')
        CALL VF_ZSTOR(DTCNST,TEXT(IS(3):IE(3)))
        IF (DTCNST.LT.ZERO) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')
        IDTTYP=0

CD    -- TIME AUTOを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'AUTO') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IITIME','SYNTAX ERROR.')
        CALL VF_ZSTOR(DTINIT,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOR(DTSAFE,TEXT(IS(4):IE(4)))
        IF (DTINIT.LT.ZERO) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')
        IF (DTSAFE.LT.ZERO) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')
        IDTTYP=1

CD    -- TIME LIMITを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'LIMIT') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IITIME','SYNTAX ERROR.')
        CALL VF_ZSTOR(DTMIN,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOR(DTMAX,TEXT(IS(4):IE(4)))
        IF (DTMIN.LT.ZERO ) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')
        IF (DTMIN.GT.DTMAX) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')

CD    -- TIME ENDを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'END') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IITIME','SYNTAX.')
        CALL VF_ZSTOI(NEND,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOR(TEND,TEXT(IS(4):IE(4)))
        IF (NEND.LT.0    ) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')
        IF (TEND.LT.0.0D0) CALL VF_A2ERR('VF_IITIME','INVALID VALUE.')

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IITIME','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
