      SUBROUTINE VF_IIGRID(ISW,NL,NUML,XYZ,IEOF,LEVEL,IS,IE,NWD,TEXT,
     &                     NUMWK,XYZWK)

CD=== 概要 ===========================================================

CDT   VF_IIGRID:ある方向の格子座標データ(GRID)を読み込み、解釈する
CD      (1)1行目はVF_II1INPより受け取る
CD      (2)「END」を読み込むまで、反復する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'

CD    -- 引数 --
CD    ISW             : IN  : I*4   : 方向(X:=1,Y:=2,Z:=3)
CD    NL              : I/O : I*4   : 今回のレベルで決めた格子数+1
CD    NUML            : I/O : I*4   : 前回のレベルで決めた格子数+1
CD    XYZ(MAXG1,NUML) : I/O : R*8   : ある方向の格子座標等
CD    IEOF            : I/O : I*4   : = 0:EOFを読み込んでいない
CD                                    !=0:EOFを読み込んだ
CD    LEVEL           : IN  : I*4   : 入力レベル
CD                                    =<0:格子数のみを決定
CD                                    = 1:座標値を読み込む
CD                                    >=2:読み飛ばす
CD    IS(MAXWDS)      : I/O : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS)      : I/O : I*4   : n番目の単語の終了位置
CD    NWD             : I/O : I*4   : 単語の数
CD    TEXT            : I/O : C*(*) : 入力した文字列
      DIMENSION XYZ(MAXG1,NUML)
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      DIMENSION XYZWK(NUMWK)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 初期設定 --
      IF (NL.NE.0) CALL VF_A2ERR('VF_IIGRID','SECOND TIMES.')
      NL  =1
      IEND=0
      IP  =3
      X1  =0.0D0
      X2  =0.0D0
C     エラーチェッカーをだますため
      XOLD=0.0D0
      IF     (ISW.EQ.1) THEN
        NUML0=NUMI0
        LG0=MYGIS
        LG2=MYGIE
      ELSEIF (ISW.EQ.2) THEN
        NUML0=NUMJ0
        LG0=MYGJS
        LG2=MYGJE
      ELSE
        NUML0=NUML
        LG0=1
        LG2=NUML0
      ENDIF
      LG1=LG0
      IF (LG1.EQ.1) LG1=2

CD    -- ENDがくるまで座標値を読む --
C     ** 中判定反復 **
 100  CONTINUE

CD      -- 単語を解釈 --
        DO 200 I=IP,NWD
          IF (TEXT(IS(I):IE(I)).EQ.'END') THEN
            IF (I.NE.NWD) CALL VF_A2ERR('VF_IIGRID','SYNTAX ERROR.')
            IEND=I
            GOTO 210
          ELSE
            NL=NL+1
            IF ((LEVEL.GE.1) .AND. (NL.GT.NUML0)) 
     &                CALL VF_A2ERR('VF_IIGRID','FILE WAS CHANGED.')
            CALL VF_ZSTOR(XNOW,TEXT(IS(I):IE(I)))
            IF (NL.GE.3) THEN
              IF (XNOW-XOLD.LT.ZEROG)
     &                  CALL VF_A2ERR('VF_IIGRID','INVALID VALUE.')
            ENDIF
            IF (LEVEL.EQ.1) THEN
              IF     (LG1.LE.NL .AND. NL.LE.LG2) THEN
                XYZ(1,NL-LG0+1)=XNOW
              ELSEIF (NL.EQ.LG2+1              ) THEN
C               * 通信をしないためにとっておく
                XBUF=XNOW
              ENDIF
cmod20160721              IF (LB_CADMAS.EQ.1) XYZWK(NL-1)=XNOW
              XYZWK(NL-1)=XNOW
            ENDIF
            XOLD=XNOW
            IF (NL.EQ.2) X1=XNOW
            X2=XNOW
          ENDIF
 200    CONTINUE
 210    CONTINUE

CD      -- ENDを読み込んだら抜ける --
        IF (IEND.NE.0) GOTO 300
        IF (IEOF.NE.0) CALL VF_A2ERR('VF_IIGRID','NOT FOUND (END).')

CD      -- 次の1行を読み込み単語に分解する --
        CALL VF_ZGETLN(IS,IE,MAXWDS,NWD,IINFIL,IEOF,TEXT)
        IF (NWD.GT.0 .AND. LEVEL.LE.0)
     &               WRITE(ILPFIL,9510) (TEXT(IS(I):IE(I)),I=1,NWD)
        IP=1

CD    ** 反復終了 **
        GOTO 100
 300  CONTINUE

CD    -- 格子数のチェックと全体の範囲 --
      IF (NL.LT.3) CALL VF_A2ERR('VF_IIGRID','2 > NUMBER OF GRID.')
      IF ((LEVEL.GE.1) .AND. (NL.NE.NUML0)) 
     &               CALL VF_A2ERR('VF_IIGRID','FILE WAS CHANGED.')
      IF     (ISW.EQ.1) THEN
        NUMI0 =NL
        GLXMIN=X1
        GLXMAX=X2
      ELSEIF (ISW.EQ.2) THEN
        NUMJ0 =NL
        GLYMIN=X1
        GLYMAX=X2
      ELSE
        NUML  =NL
      ENDIF

CD    -- 格子関連データの設定 --
      IF (LEVEL.EQ.1) CALL VF_CGRID(ISW,XYZ,XBUF,NUML)

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(' ','& ',100('[',A,']':))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
