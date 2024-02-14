      SUBROUTINE VF_IIHIDM(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIHIDM:HiDEMプログラムとの連成制御データ(HIDEM)を解釈する

C==== 宣言 ===========================================================

      use mod_dem

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIPARA','SYNTAX ERROR.')

CD    -- HIDEM YESを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'YES') THEN
        IHIDEM=1

CD    -- HIDEM NOを解釈する --
      ELSE IF(TEXT(IS(2):IE(2)).EQ.'NO') THEN
        IHIDEM=0

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIPARA','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
