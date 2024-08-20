      SUBROUTINE VF_ZSTOI(IVAL,TEXT)

CD=== 概要 ===========================================================

CDT   VF_ZSTOI:文字列を整数に変換する
CD      (1)前の空白は無視する
CD      (2)符号および数字以外の文字以降は認識しない

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    IVAL : OUT : I*4   : 変換した整数
CD    TEXT : IN  : C*(*) : 変換する文字列
      CHARACTER*(*) TEXT

C==== 実行 ===========================================================

CD    -- 初期設定 --
      IVAL=0
      ISG =1
      IP  =0
      NC  =LEN(TEXT)

CD    -- 空白でない文字の開始位置 --
      DO 100 I=1,NC
        IF (TEXT(I:I).NE.' ') THEN
          IP=I
          GOTO 110
        ENDIF
  100 CONTINUE
  110 CONTINUE

CD    -- 空白行なら終了 --
      IF (IP.EQ.0) GOTO 9000

CD    -- 符号を識別 --
      IF (TEXT(IP:IP).EQ.'+') THEN
        IP=IP+1
      ELSEIF (TEXT(IP:IP).EQ.'-') THEN
        ISG=-1
        IP=IP+1
      ENDIF

CD    -- 絶対値を識別 --
      DO 200 I=IP,NC
        IF (TEXT(I:I).EQ.'0') THEN
          IVAL=10*IVAL+0
        ELSEIF (TEXT(I:I).EQ.'1') THEN
          IVAL=10*IVAL+1
        ELSEIF (TEXT(I:I).EQ.'2') THEN
          IVAL=10*IVAL+2
        ELSEIF (TEXT(I:I).EQ.'3') THEN
          IVAL=10*IVAL+3
        ELSEIF (TEXT(I:I).EQ.'4') THEN
          IVAL=10*IVAL+4
        ELSEIF (TEXT(I:I).EQ.'5') THEN
          IVAL=10*IVAL+5
        ELSEIF (TEXT(I:I).EQ.'6') THEN
          IVAL=10*IVAL+6
        ELSEIF (TEXT(I:I).EQ.'7') THEN
          IVAL=10*IVAL+7
        ELSEIF (TEXT(I:I).EQ.'8') THEN
          IVAL=10*IVAL+8
        ELSEIF (TEXT(I:I).EQ.'9') THEN
          IVAL=10*IVAL+9
        ELSE
          GOTO 210
        ENDIF
 200  CONTINUE
 210  CONTINUE

CD    -- 符号*絶対値を計算 --
      IVAL=ISG*IVAL

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
