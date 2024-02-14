      SUBROUTINE VF_ZGETLN(IS,IE,MWD,NWD,IFIL,IEOF,TEXT)

CD=== 概要 ===========================================================

CDT   VF_ZGETLN:テキストデータを1行を読み込み、単語に分解する
CD      (1)区切りは1つ以上の空白
CD      (2)「#」以降はコメント
CD      (3)n番目の単語は TEXT(IS(n):IE(n)) の形式で参照する
CD      (4)空白行とコメント行はNWD=0
CD      (5)ダブ等の特殊文字は判定していない(注意)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    IS(MWD) : OUT : I*4   : n番目の単語の開始位置
CD    IE(MWD) : OUT : I*4   : n番目の単語の終了位置
CD    MWD     : IN  : I*4   : 単語の最大数
CD    NWD     : OUT : I*4   : 単語の数
CD    IFIL    : IN  : I*4   : ファイル番号
CD    IEOF    : OUT : I*4   : = 0:EOFを読み込んでいない
CD                            !=0:EOFを読み込んだ
CD    TEXT    : OUT : C*(*) : 入力した文字列
      DIMENSION IS(MWD),IE(MWD)
      CHARACTER*(*) TEXT
C
      CHARACTER*(MAXCHR) LINBUF(MAXLIN)
      SAVE LINBUF
      INTEGER LINCNT,LINEOF
      DATA LINCNT,LINEOF /2*0/
C
C     LINBUF: 入力データMAXLIN行分を読み貯めておく配列
C     LINCNT: 前回の呼び出しで読み込んだLINBUFの要素番号
C             1<=LINCNT<MAXLINでMAXLINに達したら0リセット
C             =0のときLINBUFを読み込む
C     LINEOF: EOFに達したときのLINBUFの要素番号。EOFに達していないときは0

C==== 実行 ===========================================================

CD    -- 初期設定 --
      NWD =0
      IEOF=0
      TEXT=' '
      NC  =LEN(TEXT)

CD    -- MAXLIN行を読み込む --
      IF (LINCNT.EQ.0) THEN
        LINBUF(:)=' '
        IF (MYRANK.EQ.0.OR.INMODE.EQ.1) THEN
          DO N=1,MAXLIN
            READ(IFIL,9510,END=100,ERR=9010) LINBUF(N)
          ENDDO
          LINEOF=0
          GOTO 110
 100      CONTINUE
          LINEOF=N
 110      CONTINUE
        ENDIF
        IF (INMODE.EQ.0.AND.NPROCS.NE.1) THEN
          CALL VF_P1BCSC(LINBUF,NC*MAXLIN,0)
          CALL VF_P1BCSI(LINEOF,        1,0)
        ENDIF
      ENDIF
C
      LINCNT=LINCNT+1
      TEXT=LINBUF(LINCNT)
      IF (LINCNT.EQ.LINEOF) IEOF=1
      IF (LINCNT.EQ.MAXLIN) LINCNT=0

CD    -- 空白行とコメント行はスキップ --
      ION=0
      ICM=0
      DO 200 I=1,NC
        IF (ION.EQ.0 .AND. TEXT(I:I).NE.' ') ION=I
        IF (ICM.EQ.0 .AND. TEXT(I:I).EQ.'#') ICM=I
 200  CONTINUE
      IF (ION.EQ.0  ) GOTO 9000
      IF (ION.EQ.ICM) GOTO 9000

CD    -- 有効行を単語に分解 --
      IF (ICM.EQ.0) ICM=NC+1
      TEXT=TEXT(ION:ICM-1)
      CALL VF_ZSTOWS(IS,IE,MWD,NWD,TEXT(1:ICM-ION))

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_ZGETLN','I/O ERROR(text data).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(A)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
