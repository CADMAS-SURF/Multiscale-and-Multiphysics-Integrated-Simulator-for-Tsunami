      SUBROUTINE VF_IIOPT(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIOPT:オプションデータ(OPTION)を解釈する

C==== 宣言 ===========================================================

      USE MOD_FAULT, ONLY: set_utm,icoord,isystem,jsystem
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      INCLUDE 'VF_ACOMPR.h'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
C----------------------------------------------------------2016.09 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2016.09 end

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT
      DOUBLE PRECISION LC_DEG

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')

CD    -- OPTION SUB-LOOPを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'SUB-LOOP') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        CALL VF_ZSTOI(LOOPS,TEXT(IS(3):IE(3)))
        IF (LOOPS.LT.1) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')

CD    -- OPTION S-CELL-VELを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'S-CELL-VEL') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'DU=0' ) THEN
          IBSUW0=1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'D2U=0') THEN
          IBSUW0=0
        ELSE
          CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
        ENDIF

CD    -- OPTION T-DOORを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'T-DOOR') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'BUB' ) THEN
          IF (TEXT(IS(4):IE(4)).EQ.'OFF' ) THEN
            WBUB=0.0D0
          ELSE
            CALL VF_ZSTOR(WBUB,TEXT(IS(4):IE(4)))
            IF (WBUB.LT.ZERO) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')
          ENDIF
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'DROP') THEN
          IF     (TEXT(IS(4):IE(4)).EQ.'OFF'         ) THEN
            IDROP=0
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'FREE-RUNDOWN') THEN
            IDROP=1
          ELSE
            CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
          ENDIF
        ELSE
          CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD    -- OPTION STATEを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'STATE') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        CALL VF_ZSTOI(ISTATE,TEXT(IS(3):IE(3)))
        IF (ISTATE.GT.1) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')
        DO I=4,NWD
          CALL VF_ZSTOR(SPARAM(I-3),TEXT(IS(I):IE(I)))
        END DO

CD    -- OPTION DENS-ERRORを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'DENS-ERROR') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        CALL VF_ZSTOR(SERROR,TEXT(IS(3):IE(3)))
        IF (SERROR.LE.ZERO) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')
        IF (NWD.GE.4) THEN
        CALL VF_ZSTOI(MAXITR,TEXT(IS(4):IE(4)))
        IF (MAXITR.LE.0) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')
        ENDIF
        IF (NWD.GE.5) THEN
        CALL VF_ZSTOR(SRELAX,TEXT(IS(5):IE(5)))
        IF (SRELAX.LT.ZERO) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C----------------------------------------------------------2016.09 start
CD    -- OPTION SEA-BOTTOMを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'SEA-BOTTOM') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        IF (TEXT(IS(3):IE(3)).EQ.'ON' ) THEN
          ISEABT = 1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'OFF' ) THEN
          ISEABT = 0
cadd 20130703(s)
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'CALC' ) THEN
          ISEABT = -1
cadd 20130703(e)
        ELSE
          CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
        ENDIF
        
CD    -- OPTION COORDINATEを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'COORDINATE') THEN
        IF (NWD.LT.4) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'JAPAN-PLANE-RECTANGULAR' ) THEN
          CALL VF_ZSTOI(ICOORD,TEXT(IS(4):IE(4)))
          IF (ICOORD.LT.1.OR.ICOORD.GT.19)
     $      CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'UTM') THEN
          CALL VF_ZSTOR(LC_DEG,TEXT(IS(4):IE(4)))
          CALL SET_UTM(LC_DEG)
        ELSE
          CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
        ENDIF

CD    -- OPTION FAULT-SYSTEMを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'FAULT-SYSTEM') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'TOKYO' ) THEN
          JSYSTEM=1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'JGD2000') THEN
          JSYSTEM=2
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'WGS84') THEN
          JSYSTEM=3
        ELSE
          CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
        ENDIF
        
CD    -- OPTION GRID-SYSTEMを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'GRID-SYSTEM') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'TOKYO' ) THEN
          ISYSTEM=1
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'JGD2000') THEN
          ISYSTEM=2
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'WGS84') THEN
          ISYSTEM=3
        ELSE
          CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
        ENDIF

C----------------------------------------------------------2016.09 end
CD    -- OPTION MAX-VELOCITYを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'MAX-VELOCITY') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIOPT','SYNTAX ERROR.')
        CALL VF_ZSTOR(VVMAX,TEXT(IS(3):IE(3)))
        IF (VVMAX.LE.ZERO ) CALL VF_A2ERR('VF_IIOPT','INVALID VALUE.')

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIOPT','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
