      SUBROUTINE VF_ZSTOWS(IS,IE,MWD,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_ZSTOWS:文字列を単語に分解する
CD      (1)区切りは1つ以上の空白
CD      (2)n番目の単語は TEXT(IS(n):IE(n)) の形式で参照する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    IS(MWD)  : OUT : I*4   : n番目の単語の開始位置
CD    IE(MWD)  : OUT : I*4   : n番目の単語の終了位置
CD    MWD      : IN  : I*4   : 単語の最大数
CD    NWD      : OUT : I*4   : 単語の数
CD    TEXT     : IN  : C*(*) : 分解する文字列
      DIMENSION IS(MWD),IE(MWD)
      CHARACTER*(*) TEXT

C==== 実行 ===========================================================

CD    -- 初期設定 --
      NWD=0

CD    -- 単語の最大数がゼロ以下なら終了 --
      IF (MWD.LE.0) GOTO 9000

CD    -- 1行の文字数がゼロ以下なら終了 --
      NC=LEN(TEXT)
      IF (NC.LE.0) GOTO 9000

CD    -- 文字列を分解 --
      ION=0
      DO 100 I=1,NC
        IF (TEXT(I:I).EQ.' ') THEN
          IF (ION.NE.0) THEN
            IE(NWD)=I-1
            ION=0
            IF (NWD.GE.MWD) GOTO 110
          ENDIF
        ELSE
          IF (ION.EQ.0) THEN
            NWD=NWD+1
            IS(NWD)=I
            ION=1
          ENDIF
        ENDIF
 100  CONTINUE
 110  CONTINUE
      IF (ION.NE.0) IE(NWD)=NC

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
