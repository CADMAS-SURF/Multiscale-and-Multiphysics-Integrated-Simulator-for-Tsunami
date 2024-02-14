      SUBROUTINE VF_IIEQUA(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIEQUA:方程式制御データ(EQUATION)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIEQUA','SYNTAX ERROR.')

CD    -- EQUATION K-EPSを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'K-EPS') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIEQUA','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'NOCALC') THEN
          LEQK=0
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'CALC'  ) THEN
          LEQK=1
        ELSE
          CALL VF_A2ERR('VF_IIEQUA','UNKNOWN WORD.')
        ENDIF

CD    -- EQUATION TEMPERATUREを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'TEMPERATURE') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIEQUA','SYNTAX ERROR.')
        IF     (TEXT(IS(3):IE(3)).EQ.'NOCALC') THEN
          LEQT=0
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'CALC'  ) THEN
          LEQT=1
        ELSE
          CALL VF_A2ERR('VF_IIEQUA','UNKNOWN WORD.')
        ENDIF

CD    -- EQUATION CONCENTRATIONを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'CONCENTRATION') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIEQUA','SYNTAX ERROR.')
        CALL VF_ZSTOI(LEQC,TEXT(IS(3):IE(3)))
        IF (LEQC.LT.0    ) CALL VF_A2ERR('VF_IIEQUA','INVALID VALUE.')
        IF (LEQC.GT.MAXNC) CALL VF_A2ERR('VF_IIEQUA','INVALID VALUE.')

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIEQUA','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
