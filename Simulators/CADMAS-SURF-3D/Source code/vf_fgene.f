      SUBROUTINE VF_FGENE(XX,YY,FF,SRCUV,QF,NF)

CD=== 概要 ===========================================================

CDT   VF_FEULER:VOF関数Fの生成消滅項を計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    SRCUV(NUMIJ,NUMK) : IN  : R*8 : 造波ソースのための流速
CD    QF(@FOR-3D@)     : IN  : R*8 : VOF関数Fの生成消滅
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
      DIMENSION FF(NUMI,NUMJ,NUMK),SRCUV(NUMIJ,NUMK)
      DIMENSION QF(NUMI,NUMJ,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 造波ソースのための定数 --
      IWS=0
      JWS=0
      IF     (ISCTYP(1).GT.0) THEN
        IWS= ISCTYP(1)-(MYGIS-1)
      ELSEIF (ISCTYP(1).LT.0) THEN
        JWS=-ISCTYP(1)-(MYGJS-1)
      ENDIF

CD    -- 時間積分 --
      DO 210 J=MYJS,MYJE
        DO 200 I=MYIS,MYIE
          DO 100 K=2,NUMK-1
            IF (IWS.EQ.I) THEN
              IF     (NF(I,J,K).EQ.-1) THEN
              ELSEIF (NF(I,J,K).EQ. 0) THEN
                Q=2.0D0*XX(4,I)*SRCUV(J,K)
                QF(I,J,K)=QF(I,J,K)+FF(I,J,K)*Q
              ELSEIF (NF(I,J,K).NE.8) THEN
                Q=2.0D0*XX(4,I)*SRCUV(J,K)
                QF(I,J,K)=QF(I,J,K)+FF(I,J,K)*Q
                GOTO 110
              ENDIF
            ENDIF
            IF (JWS.EQ.J) THEN
              IF     (NF(I,J,K).EQ.-1) THEN
              ELSEIF (NF(I,J,K).EQ. 0) THEN
                Q=2.0D0*YY(4,J)*SRCUV(I,K)
                QF(I,J,K)=QF(I,J,K)+FF(I,J,K)*Q
              ELSEIF (NF(I,J,K).NE.8) THEN
                Q=2.0D0*YY(4,J)*SRCUV(I,K)
                QF(I,J,K)=QF(I,J,K)+FF(I,J,K)*Q
                GOTO 110
              ENDIF
            ENDIF
 100      CONTINUE
 110      CONTINUE
 200    CONTINUE
 210  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
