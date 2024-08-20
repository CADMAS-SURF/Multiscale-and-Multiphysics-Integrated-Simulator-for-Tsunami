      SUBROUTINE VF_CFXYZ(XX,YY,ZZ,FF,FX,FY,FZ,GGV,
     &                    DBUF,NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_CFXYZ:x,y,z各方向のスタッガードセルにおけるF値を設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    FX(@FOR-3D@)     : I/O : R*8 : x方向スタッガードセルでのVOF関数Fx
CD    FY(@FOR-3D@)     : I/O : R*8 : y方向スタッガードセルでのVOF関数Fy
CD    FZ(@FOR-3D@)     : I/O : R*8 : z方向スタッガードセルでのVOF関数Fz
CD    GGV(@FOR-3D@)       : IN  : R*8 : 空隙率
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),FX  (NUMI,NUMJ,NUMK)
      DIMENSION FY  (NUMI,NUMJ,NUMK),FZ  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

C     * MODE =1:スタッガードセル内のF値を求める *
C     * MODE<>1:線形近似でF値を求める *
      MODE=1

CD    -- x方向流速を設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=IA+1
      DO 120 K=2,NUMK-1
        DO 110 J=JA,JB
          DO 100 I=IA,IB
C           * 通常の面
            IF (INDX(I,J,K).EQ.0) THEN
              I1=I-1
              NL=NF(I1,J,K)
              NR=NF(I ,J,K)
C             * スタッガードセルVOF関数計算 *
              IF (MODE.EQ.1) THEN
C               * 両側とも液相セルの場合
                IF     (NL.EQ.0 .AND. NR.EQ.0) THEN
                  FX(I,J,K)=1.0D0
C               * 両側とも気相セルの場合
                ELSEIF (NL.EQ.8 .AND. NR.EQ.8) THEN
                  FX(I,J,K)=0.0D0
C               * それ以外
                ELSE
                  IF     (NL.EQ.0) THEN
                    FFL=1.0D0
                  ELSEIF (NL.EQ.8) THEN
                    FFL=0.0D0
                  ELSEIF (NL.EQ.1) THEN
                    FFL=MAX(0.0D0,FF(I1,J,K)-0.5D0)*2.0D0
                  ELSEIF (NL.EQ.2) THEN
                    FFL=MIN(0.5D0,FF(I1,J,K))*2.0D0
                  ELSE
                    FFL=FF(I1,J,K)
                  ENDIF
                  IF     (NR.EQ.0) THEN
                    FFR=1.0D0
                  ELSEIF (NR.EQ.8) THEN
                    FFR=0.0D0
                  ELSEIF (NR.EQ.1) THEN
                    FFR=MIN(0.5D0,FF(I,J,K))*2.0D0
                  ELSEIF (NR.EQ.2) THEN
                    FFR=MAX(0.0D0,FF(I,J,K)-0.5D0)*2.0D0
                  ELSE
                    FFR=FF(I,J,K)
                  ENDIF
                  VL=XX(2,I1)*GGV(I1,J,K)
                  VR=XX(2,I )*GGV(I ,J,K)
                  FX(I,J,K)=(VL*FFL+VR*FFR)/(VL+VR)
                ENDIF
C             * 線形近似 *
              ELSE
                FFL=FF(I1,J,K)
                FFR=FF(I,J,K)
                VL=XX(2,I1)
                VR=XX(2,I )
                FX(I,J,K)=(VL*FFL+VR*FFR)/(VL+VR)
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- y方向流速を設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMJS.EQ.1) JA=JA+1
      DO 220 K=2,NUMK-1
        DO 210 J=JA,JB
          DO 200 I=IA,IB
C           * 通常の面
            IF (INDY(I,J,K).EQ.0) THEN
              J1=J-1
              NL=NF(I,J1,K)
              NR=NF(I,J ,K)
C             * スタッガードセルVOF関数計算 *
              IF (MODE.EQ.1) THEN
C               * 両側とも液相セルの場合
                IF     (NL.EQ.0 .AND. NR.EQ.0) THEN
                  FY(I,J,K)=1.0D0
C               * 両側とも気相セルの場合
                ELSEIF (NL.EQ.8 .AND. NR.EQ.8) THEN
                  FY(I,J,K)=0.0D0
C               * それ以外
                ELSE
                  IF     (NL.EQ.0) THEN
                    FFL=1.0D0
                  ELSEIF (NL.EQ.8) THEN
                    FFL=0.0D0
                  ELSEIF (NL.EQ.3) THEN
                    FFL=MAX(0.0D0,FF(I,J1,K)-0.5D0)*2.0D0
                  ELSEIF (NL.EQ.4) THEN
                    FFL=MIN(0.5D0,FF(I,J1,K))*2.0D0
                  ELSE
                    FFL=FF(I,J1,K)
                  ENDIF
                  IF     (NR.EQ.0) THEN
                    FFR=1.0D0
                  ELSEIF (NR.EQ.8) THEN
                    FFR=0.0D0
                  ELSEIF (NR.EQ.3) THEN
                    FFR=MIN(0.5D0,FF(I,J,K))*2.0D0
                  ELSEIF (NR.EQ.4) THEN
                    FFR=MAX(0.0D0,FF(I,J,K)-0.5D0)*2.0D0
                  ELSE
                    FFR=FF(I,J,K)
                  ENDIF
                  VL=YY(2,J1)*GGV(I,J1,K)
                  VR=YY(2,J )*GGV(I,J ,K)
                  FY(I,J,K)=(VL*FFL+VR*FFR)/(VL+VR)
                ENDIF
C             * 線形近似 *
              ELSE
                FFL=FF(I,J1,K)
                FFR=FF(I,J,K)
                VL=YY(2,J1)
                VR=YY(2,J )
                FY(I,J,K)=(VL*FFL+VR*FFR)/(VL+VR)
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- z方向流速を設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      DO 320 K=3,NUMK-1
        DO 310 J=JA,JB
          DO 300 I=IA,IB
C           * 通常の面
            IF (INDZ(I,J,K).EQ.0) THEN
              K1=K-1
              NL=NF(I,J,K1)
              NR=NF(I,J,K )
C             * スタッガードセルVOF関数計算 *
                IF (MODE.EQ.1) THEN
C               * 両側とも液相セルの場合
                IF     (NL.EQ.0 .AND. NR.EQ.0) THEN
                  FZ(I,J,K)=1.0D0
C               * 両側とも気相セルの場合
                ELSEIF (NL.EQ.8 .AND. NR.EQ.8) THEN
                  FZ(I,J,K)=0.0D0
C               * それ以外
                ELSE
                  IF     (NL.EQ.0) THEN
                    FFL=1.0D0
                  ELSEIF (NL.EQ.8) THEN
                    FFL=0.0D0
                  ELSEIF (NL.EQ.5) THEN
                    FFL=MAX(0.0D0,FF(I,J,K1)-0.5D0)*2.0D0
                  ELSEIF (NL.EQ.6) THEN
                    FFL=MIN(0.5D0,FF(I,J,K1))*2.0D0
                  ELSE
                    FFL=FF(I,J,K1)
                  ENDIF
                  IF     (NR.EQ.0) THEN
                    FFR=1.0D0
                  ELSEIF (NR.EQ.8) THEN
                    FFR=0.0D0
                  ELSEIF (NR.EQ.5) THEN
                    FFR=MIN(0.5D0,FF(I,J,K))*2.0D0
                  ELSEIF (NR.EQ.6) THEN
                    FFR=MAX(0.0D0,FF(I,J,K)-0.5D0)*2.0D0
                    FFR=1.0D0
                  ELSE
                    FFR=FF(I,J,K)
                  ENDIF
                  VL=ZZ(2,K1)*GGV(I,J,K1)
                  VR=ZZ(2,K )*GGV(I,J,K )
                  FZ(I,J,K)=(VL*FFL+VR*FFR)/(VL+VR)
                ENDIF
C             * 線形近似 *
              ELSE
                FFL=FF(I,J,K)
                FFR=FF(I,J,K1)
                VL=ZZ(2,K1)
                VR=ZZ(2,K )
                FZ(I,J,K)=(VL*FFL+VR*FFR)/(VL+VR)
              ENDIF
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

      CALL VF_P3SRD2(FX,DBUF,1)
      CALL VF_P3SRD2(FY,DBUF,2)
      CALL VF_P3SRD2(FZ,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
