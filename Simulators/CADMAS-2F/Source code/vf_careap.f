      SUBROUTINE VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,ISW,VAL,
     &                     I1IN,J1IN,K1IN,I2IN,J2IN,K2IN,
     &                     VMIN,VMAX,VAV,VINT)

CD=== 概要 ===========================================================

CDT   VF_CAREAP:物理量の空間算出値を計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    ISW            : IN  : I*4 : 物理量の種別
CD                                  =1:x方向流速
CD                                  =2:y方向流速
CD                                  =3:z方向流速
CD                                  =4:圧力
CD                                  =5:F値
CD                                  =6:その他のスカラー量
CD    VAL(@FOR-3D@)  : IN  : R*8 : 物理量の種別
CD    I1IN           : IN  : I*4 : 始点のx方向セル番号
CD    J1IN           : IN  : I*4 : 始点のy方向セル番号
CD    K1IN           : IN  : I*4 : 始点のz方向セル番号
CD    I2IN           : IN  : I*4 : 終点のx方向セル番号
CD    J2IN           : IN  : I*4 : 終点のy方向セル番号
CD    K2IN           : IN  : I*4 : 終点のz方向セル番号
CD    VMIN           : OUT : R*8 : 最小値
CD    VMAX           : OUT : R*8 : 最大値
CD    VAV            : OUT : R*8 : 面積平均値
CD    VINT           : OUT : R*8 : 面積積分値
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK),FF(NUMI,NUMJ,NUMK)
      DIMENSION VAL(NUMI,NUMJ,NUMK)
      DIMENSION NF (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分等 --
      I1=I1IN-(MYGIS-1)
      I2=I2IN-(MYGIS-1)
      J1=J1IN-(MYGJS-1)
      J2=J2IN-(MYGJS-1)
      K1=K1IN
      K2=K2IN
      I1=MAX(I1,MYIS)
      I2=MIN(I2,MYIE)
      J1=MAX(J1,MYJS)
      J2=MIN(J2,MYJE)

CD    -- 初期化 --
      IP  =0
      VMIN=0.0D0
      VMAX=0.0D0
      VAV =0.0D0
      VINT=0.0D0

CD    -- x方向流速 --
      IF     (ISW.EQ.1) THEN
        DO 120 K=K1,K2
          DO 110 J=J1,J2
            DO 100 I=I1,I2
              IF (NF(I,J,K).NE.-1 .AND. NF(I,J,K).NE.8) THEN
                IP=IP+1
                P1=VAL(I  ,J,K)
                P2=VAL(I+1,J,K)
                IF (IP.EQ.1) THEN
                  VMIN=MIN(P1,P2)
                  VMAX=MAX(P1,P2)
                ELSE
                  VMIN=MIN(VMIN,P1,P2)
                  VMAX=MAX(VMAX,P1,P2)
                ENDIF
                AR  =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)*FF(I,J,K)
                VAV =VAV +AR
                VINT=VINT+AR*(P1+P2)*0.5D0
              ENDIF
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE

CD    -- y方向流速 --
      ELSEIF (ISW.EQ.2) THEN
        DO 220 K=K1,K2
          DO 210 J=J1,J2
            DO 200 I=I1,I2
              IF (NF(I,J,K).NE.-1 .AND. NF(I,J,K).NE.8) THEN
                IP=IP+1
                P1=VAL(I,J  ,K)
                P2=VAL(I,J+1,K)
                IF (IP.EQ.1) THEN
                  VMIN=MIN(P1,P2)
                  VMAX=MAX(P1,P2)
                ELSE
                  VMIN=MIN(VMIN,P1,P2)
                  VMAX=MAX(VMAX,P1,P2)
                ENDIF
                AR  =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)*FF(I,J,K)
                VAV =VAV +AR
                VINT=VINT+AR*(P1+P2)*0.5D0
              ENDIF
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE

CD    -- z方向流速 --
      ELSEIF (ISW.EQ.3) THEN
        DO 320 K=K1,K2
          DO 310 J=J1,J2
            DO 300 I=I1,I2
              IF (NF(I,J,K).NE.-1 .AND. NF(I,J,K).NE.8) THEN
                IP=IP+1
                P1=VAL(I,J,K  )
                P2=VAL(I,J,K+1)
                IF (IP.EQ.1) THEN
                  VMIN=MIN(P1,P2)
                  VMAX=MAX(P1,P2)
                ELSE
                  VMIN=MIN(VMIN,P1,P2)
                  VMAX=MAX(VMAX,P1,P2)
                ENDIF
                AR  =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)*FF(I,J,K)
                VAV =VAV +AR
                VINT=VINT+AR*(P1+P2)*0.5D0
              ENDIF
 300        CONTINUE
 310      CONTINUE
 320    CONTINUE

CD    -- F値 --
      ELSEIF (ISW.EQ.5) THEN
        DO 420 K=K1,K2
          DO 410 J=J1,J2
            DO 400 I=I1,I2
              IF (NF(I,J,K).NE.-1) THEN
                IP=IP+1
                IF (IP.EQ.1) THEN
                  VMIN=FF(I,J,K)
                  VMAX=FF(I,J,K)
                ELSE
                  VMIN=MIN(VMIN,FF(I,J,K))
                  VMAX=MAX(VMAX,FF(I,J,K))
                ENDIF
                AR  =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
                VAV =VAV +AR
                VINT=VINT+AR*FF(I,J,K)
              ENDIF
 400        CONTINUE
 410      CONTINUE
 420    CONTINUE

CD    -- その他のスカラー量(含む圧力) --
      ELSE
        DO 520 K=K1,K2
          DO 510 J=J1,J2
            DO 500 I=I1,I2
              IF (NF(I,J,K).NE.-1 .AND. NF(I,J,K).NE.8) THEN
                IP=IP+1
                IF (IP.EQ.1) THEN
                  VMIN=VAL(I,J,K)
                  VMAX=VAL(I,J,K)
                ELSE
                  VMIN=MIN(VMIN,VAL(I,J,K))
                  VMAX=MAX(VMAX,VAL(I,J,K))
                ENDIF
                AR  =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)*FF(I,J,K)
                VAV =VAV +AR
                VINT=VINT+AR*VAL(I,J,K)
              ENDIF
 500        CONTINUE
 510      CONTINUE
 520    CONTINUE

      ENDIF

CD    -- 値を集める(CAKIY MIN,MAXは規定値あり) --
      IF (IP.NE.0) THEN
        W=VMIN
        CALL VF_P1MIND(W,VMIN)
        W=VMAX
        CALL VF_P1MAXD(W,VMAX)
        W=VAV
        CALL VF_P1SUMD(W,VAV)
        W=VINT
        CALL VF_P1SUMD(W,VINT)
        IF (VAV.GT.0.0D0) VAV=VINT/VAV
      ELSE
        W= 1.0D+30
        CALL VF_P1MIND(W,VMIN)
        W=-1.0D+30
        CALL VF_P1MAXD(W,VMAX)
        W=0.0D0
        CALL VF_P1SUMD(W,VAV)
        W=0.0D0
        CALL VF_P1SUMD(W,VINT)
        IF (VAV.GT.0.0D0) VAV=VINT/VAV
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
