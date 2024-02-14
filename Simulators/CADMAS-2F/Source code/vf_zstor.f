      SUBROUTINE VF_ZSTOR(VAL,TEXT)

CD=== 概要 ===========================================================

CDT   VF_ZSTOR:文字列を実数に変換する
CD      (1)前の空白は無視する
CD      (2)符号および数字以外の文字以降は認識しない

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    VAL  : OUT : R*8   : 変換した実数
CD    TEXT : IN  : C*(*) : 変換する文字列
      CHARACTER*(*) TEXT

C==== 実行 ===========================================================

CD    -- 初期設定 --
      VAL=0.0D0
      SG =1.0D0
      PW =1.0D0
      IP =0
      NC =LEN(TEXT)

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
        SG=-1.0D0
        IP=IP+1
      ENDIF

CD    -- 絶対値を識別 --
      IONP=0
      IOND=0
      DO 200 I=IP,NC
        IF (TEXT(I:I).EQ.'0') THEN
          VAL=10.0D0*VAL+0.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'1') THEN
          VAL=10.0D0*VAL+1.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'2') THEN
          VAL=10.0D0*VAL+2.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'3') THEN
          VAL=10.0D0*VAL+3.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'4') THEN
          VAL=10.0D0*VAL+4.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'5') THEN
          VAL=10.0D0*VAL+5.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'6') THEN
          VAL=10.0D0*VAL+6.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'7') THEN
          VAL=10.0D0*VAL+7.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'8') THEN
          VAL=10.0D0*VAL+8.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'9') THEN
          VAL=10.0D0*VAL+9.0D0
          IF (IONP.NE.0) PW=PW*10.0D0
        ELSEIF (TEXT(I:I).EQ.'.') THEN
          IF (IONP.NE.0) GOTO 210
          IONP=1
        ELSEIF (TEXT(I:I).EQ.'D' .OR. TEXT(I:I).EQ.'d' .OR.
     &          TEXT(I:I).EQ.'E' .OR. TEXT(I:I).EQ.'e'     ) THEN
          IOND=I+1
          GOTO 210
        ELSE
          GOTO 210
        ENDIF
 200  CONTINUE
 210  CONTINUE

CD    -- 符号*絶対値を計算 --
      VAL=SG*(VAL/PW)

CD    -- 指数部を識別 --
      IF (IOND.GE.1 .AND. IOND.LE.NC) THEN
        CALL VF_ZSTOI(I,TEXT(IOND:NC))
        VAL=VAL*(10.0D0**I)
      ENDIF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
