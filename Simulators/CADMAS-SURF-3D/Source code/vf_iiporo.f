      SUBROUTINE VF_IIPORO(CM0,CD0,GGV,GGX,GGY,GGZ,
     &                     NF,INDX,INDY,INDZ,IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIPORO:ポーラス関連データ(POROUS)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    CM0(@FOR-3D@)    : I/O : R*8 : 慣性力係数
CD    CD0(@FOR-3D@)    : I/O : R*8 : 抵抗係数
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    IS(MAXWDS)       : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS)       : IN  : I*4   : n番目の単語の終了位置
CD    NWD              : IN  : I*4   : 単語の数
CD    TEXT             : IN  : C*(*) : 入力した文字列
      DIMENSION CM0 (NUMI,NUMJ,NUMK),CD0 (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')

CD    -- POROUS LIMを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'LIM') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOR(PLOWER,TEXT(IS(3):IE(3)))
        IF (PLOWER.LT.ZERO ) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (PLOWER.GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')

CD    -- POROUS Vを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'V') THEN
        IF (NWD.LT.10) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS( 3):IE( 3)))
        CALL VF_ZSTOI(J1,TEXT(IS( 4):IE( 4)))
        CALL VF_ZSTOI(K1,TEXT(IS( 5):IE( 5)))
        CALL VF_ZSTOI(I2,TEXT(IS( 6):IE( 6)))
        CALL VF_ZSTOI(J2,TEXT(IS( 7):IE( 7)))
        CALL VF_ZSTOI(K2,TEXT(IS( 8):IE( 8)))
        CALL VF_ZSTOR(V1,TEXT(IS( 9):IE( 9)))
        CALL VF_ZSTOR(V2,TEXT(IS(10):IE(10)))
        VAL=MAX(V2+(1.0D0-V2)*V1,PLOWER)
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                    CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .LT.0.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.LT.ZERO ) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        DO 120 K=K1+1,K2+1
          DO 110 J=J1+1,J2+1
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 100 I=I1+1,I2+1
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (NF(I-IP,J-JP,K).NE.-1) GGV(I-IP,J-JP,K)=VAL
                ENDIF
 100          CONTINUE
            ENDIF
 110      CONTINUE
 120    CONTINUE

CD    -- POROUS Xを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'X') THEN
        IF (NWD.LT.10) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS( 3):IE( 3)))
        CALL VF_ZSTOI(J1,TEXT(IS( 4):IE( 4)))
        CALL VF_ZSTOI(K1,TEXT(IS( 5):IE( 5)))
        CALL VF_ZSTOI(I2,TEXT(IS( 6):IE( 6)))
        CALL VF_ZSTOI(J2,TEXT(IS( 7):IE( 7)))
        CALL VF_ZSTOI(K2,TEXT(IS( 8):IE( 8)))
        CALL VF_ZSTOR(V1,TEXT(IS( 9):IE( 9)))
        CALL VF_ZSTOR(V2,TEXT(IS(10):IE(10)))
        VAL=MAX(V2+(1.0D0-V2)*V1,PLOWER)
        IF (I1.LT.1  .OR. I2.GT.NUMI0-1 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                    CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .LT.0.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.LT.ZERO ) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        DO 220 K=K1+1,K2+1
          DO 210 J=J1+1,J2+1
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 200 I=I1+1,I2+1
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (INDX(I-IP,J-JP,K).NE.-1) GGX(I-IP,J-JP,K)=VAL
                ENDIF
 200          CONTINUE
            ENDIF
 210      CONTINUE
 220    CONTINUE

CD    -- POROUS Yを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'Y') THEN
        IF (NWD.LT.10) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS( 3):IE( 3)))
        CALL VF_ZSTOI(J1,TEXT(IS( 4):IE( 4)))
        CALL VF_ZSTOI(K1,TEXT(IS( 5):IE( 5)))
        CALL VF_ZSTOI(I2,TEXT(IS( 6):IE( 6)))
        CALL VF_ZSTOI(J2,TEXT(IS( 7):IE( 7)))
        CALL VF_ZSTOI(K2,TEXT(IS( 8):IE( 8)))
        CALL VF_ZSTOR(V1,TEXT(IS( 9):IE( 9)))
        CALL VF_ZSTOR(V2,TEXT(IS(10):IE(10)))
        VAL=MAX(V2+(1.0D0-V2)*V1,PLOWER)
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-1 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                    CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .LT.0.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.LT.ZERO ) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        DO 320 K=K1+1,K2+1
          DO 310 J=J1+1,J2+1
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 300 I=I1+1,I2+1
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (INDY(I-IP,J-JP,K).NE.-1) GGY(I-IP,J-JP,K)=VAL
                ENDIF
 300          CONTINUE
            ENDIF
 310      CONTINUE
 320    CONTINUE

CD    -- POROUS Zを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'Z') THEN
        IF (NWD.LT.10) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS( 3):IE( 3)))
        CALL VF_ZSTOI(J1,TEXT(IS( 4):IE( 4)))
        CALL VF_ZSTOI(K1,TEXT(IS( 5):IE( 5)))
        CALL VF_ZSTOI(I2,TEXT(IS( 6):IE( 6)))
        CALL VF_ZSTOI(J2,TEXT(IS( 7):IE( 7)))
        CALL VF_ZSTOI(K2,TEXT(IS( 8):IE( 8)))
        CALL VF_ZSTOR(V1,TEXT(IS( 9):IE( 9)))
        CALL VF_ZSTOR(V2,TEXT(IS(10):IE(10)))
        VAL=MAX(V2+(1.0D0-V2)*V1,PLOWER)
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -1 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                    CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .LT.0.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (V1 .GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.LT.ZERO ) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (VAL.GT.1.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        DO 420 K=K1+1,K2+1
          DO 410 J=J1+1,J2+1
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 400 I=I1+1,I2+1
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (INDZ(I-IP,J-JP,K).NE.-1) GGZ(I-IP,J-JP,K)=VAL
                ENDIF
 400          CONTINUE
            ENDIF
 410      CONTINUE
 420    CONTINUE

CD    -- POROUS CMを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'CM') THEN
        IF (NWD.LT.9) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOI(J1,TEXT(IS(4):IE(4)))
        CALL VF_ZSTOI(K1,TEXT(IS(5):IE(5)))
        CALL VF_ZSTOI(I2,TEXT(IS(6):IE(6)))
        CALL VF_ZSTOI(J2,TEXT(IS(7):IE(7)))
        CALL VF_ZSTOI(K2,TEXT(IS(8):IE(8)))
        CALL VF_ZSTOR(CM,TEXT(IS(9):IE(9)))
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                   CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (CM.LT.0.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        DO 520 K=K1+1,K2+1
          DO 510 J=J1+1,J2+1
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 500 I=I1+1,I2+1
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (NF(I-IP,J-JP,K).NE.-1) CM0(I-IP,J-JP,K)=CM
                ENDIF
 500          CONTINUE
            ENDIF
 510      CONTINUE
 520    CONTINUE

CD    -- POROUS CDを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'CD') THEN
        IF (NWD.LT.9) CALL VF_A2ERR('VF_IIPORO','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOI(J1,TEXT(IS(4):IE(4)))
        CALL VF_ZSTOI(K1,TEXT(IS(5):IE(5)))
        CALL VF_ZSTOI(I2,TEXT(IS(6):IE(6)))
        CALL VF_ZSTOI(J2,TEXT(IS(7):IE(7)))
        CALL VF_ZSTOI(K2,TEXT(IS(8):IE(8)))
        CALL VF_ZSTOR(CD,TEXT(IS(9):IE(9)))
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                   CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        IF (CD.LT.0.0D0) CALL VF_A2ERR('VF_IIPORO','INVALID VALUE.')
        DO 620 K=K1+1,K2+1
          DO 610 J=J1+1,J2+1
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 600 I=I1+1,I2+1
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (NF(I-IP,J-JP,K).NE.-1) CD0(I-IP,J-JP,K)=CD
                ENDIF
 600          CONTINUE
            ENDIF
 610      CONTINUE
 620    CONTINUE

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIPORO','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
