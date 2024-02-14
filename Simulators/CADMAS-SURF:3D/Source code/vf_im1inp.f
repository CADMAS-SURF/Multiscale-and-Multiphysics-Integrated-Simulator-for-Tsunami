      SUBROUTINE VF_IM1INP(DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT)

CD=== 概要 ===========================================================

CDT   VF_IM1INP:マトリクスデータファイル-1を読み込む

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DMTBTT(MTBTT)       : OUT : R*8 : マトリクスデータの無次元位相
CD    DMTBZZ(MTBZZ)       : OUT : R*8 : マトリクスデータのz座標
CD    DMTBHH(MTBTT)       : OUT : R*8 : マトリクスデータの水位
CD    DMTBUN(MTBZZ,MTBTT) : OUT : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT(MTBZZ,MTBTT) : OUT : R*8 : マトリクスデータの鉛直方向流速
      DIMENSION DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
      DIMENSION DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)

CD    -- 局所変数 --
CD    TEXT       : C*(MAXCHR) : 入力した文字列
CD    IS(MAXWDS) : I*4        : n番目の単語の開始位置
CD    IE(MAXWDS) : I*4        : n番目の単語の終了位置
      CHARACTER*(MAXCHR) TEXT
      DIMENSION IS(MAXWDS),IE(MAXWDS)

C==== 実行 ===========================================================

CD    -- 読み込みレベルの設定 --
      ISW=0
      IF (MTBTT.GT.0) ISW=1

CD    -- ファイルをオープンし、データ数等を読み込む --
      WRITE(ILPFIL,9510)
      IMTFIL=0
      IF (MYRANK.EQ.0) THEN
        OPEN(MFILMT,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.mtb',
     &       STATUS='OLD',FORM='FORMATTED'  )
        IMTFIL=MFILMT
      ENDIF
      IF (ISW.EQ.0) WRITE(ILPFIL,9520) '  INITIAL.'
      IEOF=0
      IP  =0
      NWD =0

C     * マトリクスデータのタイプ
      IP=IP+1
      IF (IP.GT.NWD) THEN
        CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
        IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
        IP=1
      ENDIF
      IF     (TEXT(IS(IP):IE(IP)).EQ.'LEVEL-ON'  ) THEN
        MTBTYP=1
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'LEVEL-OFF' ) THEN
        MTBTYP=2
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'LEVEL-ONLY') THEN
        MTBTYP=3
      ELSE
        CALL VF_A2ERR('VF_IM1INP','UNKNOWN WORD.')
      ENDIF

C     * 水深方向のデータ数
      IF (MTBTYP.NE.3) THEN
        IP=IP+1
        IF (IP.GT.NWD) THEN
          CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
          IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
          IP=1
        ENDIF
        CALL VF_ZSTOI(MTBZZ,TEXT(IS(IP):IE(IP)))
        IF (MTBZZ.LE.0) CALL VF_A2ERR('VF_IM1INP','INVALID VALUE.')
      ENDIF

C     * 位相方向のデータ数
      IP=IP+1
      IF (IP.GT.NWD) THEN
        CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
        IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
        IP=1
      ENDIF
      CALL VF_ZSTOI(MTBTT,TEXT(IS(IP):IE(IP)))
      IF (MTBTT.LE.0) CALL VF_A2ERR('VF_IM1INP','INVALID VALUE.')

C     * 初期無次元位相
      IP=IP+1
      IF (IP.GT.NWD) THEN
        CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
        IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
        IP=1
      ENDIF
      CALL VF_ZSTOR(DMTBT0,TEXT(IS(IP):IE(IP)))
      IF (DMTBT0.LT.0.0D0 .OR. DMTBT0.GT.1.0D0) 
     &                CALL VF_A2ERR('VF_IM1INP','INVALID VALUE.')

CD    -- その他のデータを読み込む --
      IF (ISW.NE.0) THEN

C       * z座標(平均水位をゼロ)
        IF (MTBTYP.NE.3) THEN
          DO 100 K=1,MTBZZ
            IP=IP+1
            IF (IP.GT.NWD) THEN
              CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
              IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
              IP=1
            ENDIF
            CALL VF_ZSTOR(DMTBZZ(K),TEXT(IS(IP):IE(IP)))
            IF (K.GT.1) THEN
              IF (DMTBZZ(K).LE.DMTBZZ(K-1))
     &                 CALL VF_A2ERR('VF_IM1INP','INVALID VALUE.')
            ENDIF
 100      CONTINUE
        ENDIF

C       * 位相毎データ
        DO 210 I=1,MTBTT
          IP=IP+1
          IF (IP.GT.NWD) THEN
            CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
            IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
            IP=1
          ENDIF
          IF (TEXT(IS(IP):IE(IP)).NE.'T' )
     &                    CALL VF_A2ERR('VF_IM1INP','UNKNOWN WORD.')
          IP=IP+1
          IF (IP.GT.NWD) THEN
            CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
            IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
            IP=1
          ENDIF
          CALL VF_ZSTOR(DMTBTT(I),TEXT(IS(IP):IE(IP)))
          WRITE(ILPFIL,9520) '  T=',DMTBTT(I)
          IF (DMTBTT(I).LT.0.0D0 .OR. DMTBTT(I).GT.1.0D0)
     &                    CALL VF_A2ERR('VF_IM1INP','INVALID VALUE.')
          IF (I.GT.1) THEN
            IF (DMTBTT(I).LE.DMTBTT(I-1))
     &                    CALL VF_A2ERR('VF_IM1INP','INVALID VALUE.')
          ENDIF
          IF (MTBTYP.NE.2) THEN
            IP=IP+1
            IF (IP.GT.NWD) THEN
              CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
              IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
              IP=1
            ENDIF
            CALL VF_ZSTOR(DMTBHH(I),TEXT(IS(IP):IE(IP)))
          ENDIF
          IF (MTBTYP.NE.3) THEN
            DO 200 K=1,MTBZZ
              IP=IP+1
              IF (IP.GT.NWD) THEN
                CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
                IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
                IP=1
              ENDIF
              CALL VF_ZSTOR(DMTBUN(K,I),TEXT(IS(IP):IE(IP)))
              IP=IP+1
              IF (IP.GT.NWD) THEN
                CALL VF_ZGETIM(IS,IE,MAXWDS,NWD,IMTFIL,IEOF,TEXT)
                IF (NWD.LE.0) CALL VF_A2ERR('VF_IM1INP','SYNTAX.')
                IP=1
              ENDIF
              CALL VF_ZSTOR(DMTBUT(K,I),TEXT(IS(IP):IE(IP)))
 200        CONTINUE
          ENDIF
 210    CONTINUE

      ENDIF

CD    -- ファイルをクローズする --
      IF (IMTFIL.NE.0) CLOSE(IMTFIL)
      IMTFIL=0

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_IM1INP','CAN NOT OPEN (*****.mtb).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','>> FILE-MTB : IN : ALL')
 9520 FORMAT( ' ',A,100(' ',1PE12.5:))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
