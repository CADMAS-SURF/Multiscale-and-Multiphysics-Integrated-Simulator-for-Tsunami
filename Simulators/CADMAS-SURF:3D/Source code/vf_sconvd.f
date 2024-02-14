      SUBROUTINE VF_SCONVD(ISW,XX,YY,ZZ,UU,VV,WW,CC,GGX,GGY,GGZ,BCC,
     &                     FLCU,FLCV,FLCW,NF,INDX,INDY,INDZ,INDBC)

CD=== 概要 ===========================================================

CDT   VF_SCONVD:スカラ量の移流項によるフラックスの計算(DONORスキーム)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISW              : IN  : I*4 : 計算する物理量のスイッチ
CD                                   =-1:乱流量
CD                                   = 0:温度
CD                                   >=1:濃度(成分番号)
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    CC(@FOR-3D@)     : IN  : R*8 : スカラ量
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    BCC(NUMB)        : IN  : R*8 : 境界値
CD    FLCU(@FOR-3D@)   : I/O : R*8 : x方向フラックス
CD    FLCV(@FOR-3D@)   : I/O : R*8 : y方向フラックス
CD    FLCW(@FOR-3D@)   : I/O : R*8 : z方向フラックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDBC(NUMB)      : IN  : I*4 : 境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),CC  (NUMI,NUMJ,NUMK)
      DIMENSION GGX (NUMI,NUMJ,NUMK),GGY (NUMI,NUMJ,NUMK)
      DIMENSION GGZ (NUMI,NUMJ,NUMK),BCC (NUMB)
      DIMENSION FLCU(NUMI,NUMJ,NUMK),FLCV(NUMI,NUMJ,NUMK)
      DIMENSION FLCW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDBC(NUMB)

C==== 実行 ===========================================================

CD    -- 初期設定 --
      IF     (ISW.EQ.-1) THEN
        SU=SCMK
      ELSEIF (ISW.EQ. 0) THEN
        SU=SCMT
      ELSEIF (ISW.GE. 1) THEN
        SU=SCMC(ISW)
      ELSE
        CALL VF_A2ERR('VF_SCONVD','P.G ERROR.')
      ENDIF
      SC=1.0D0-SU

CD    -- (Gx*u)*Cの計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE+1
            IF (NF(I-1,J,K)*NF(I,J,K).EQ.0) THEN
              WIN=GGX(I,J,K)*UU(I,J,K)
              IND=INDX(I,J,K)

C             * 計算格子(通常処理)
              IF     (IND.EQ.0) THEN
                FLC=WIN*XX(6,I)*( XX(2,I-1)*CC(I  ,J,K)
     &                           +XX(2,I  )*CC(I-1,J,K))
                FLCU(I,J,K)=FLCU(I,J,K)
     &                      -SU*( MAX( WIN,0.0D0)*CC(I-1,J,K)
     &                           -MAX(-WIN,0.0D0)*CC(I  ,J,K))
     &                      -SC*FLC

C             * 計算格子(特殊処理)
              ELSEIF (INDBC(IND).GE.1) THEN
                FLC=WIN*BCC(IND)
                IF (NF(I,J,K).EQ.0) THEN
                  IF (WIN.GE.0.0D0) THEN
                    FLCU(I,J,K)=FLCU(I,J,K)-FLC
                  ELSE
                    FLCU(I,J,K)=FLCU(I,J,K)-SU*WIN*CC(I  ,J,K)-SC*FLC
                  ENDIF
                ELSE
                  IF (WIN.LE.0.0D0) THEN
                    FLCU(I,J,K)=FLCU(I,J,K)-FLC
                  ELSE
                    FLCU(I,J,K)=FLCU(I,J,K)-SU*WIN*CC(I-1,J,K)-SC*FLC
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- (Gy*v)*Cの計算 --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE+1
          DO 200 I=MYIS,MYIE
            IF (NF(I,J-1,K)*NF(I,J,K).EQ.0) THEN
              WIN=GGY(I,J,K)*VV(I,J,K)
              IND=INDY(I,J,K)

C             * 計算格子(通常処理)
              IF     (IND.EQ.0) THEN
                FLC=WIN*YY(6,J)*( YY(2,J-1)*CC(I,J  ,K)
     &                           +YY(2,J  )*CC(I,J-1,K))
                FLCV(I,J,K)=FLCV(I,J,K)
     &                      -SU*( MAX( WIN,0.0D0)*CC(I,J-1,K)
     &                           -MAX(-WIN,0.0D0)*CC(I,J  ,K))
     &                      -SC*FLC

C             * 計算格子(特殊処理)
              ELSEIF (INDBC(IND).GE.1) THEN
                FLC=WIN*BCC(IND)
                IF (NF(I,J,K).EQ.0) THEN
                  IF (WIN.GE.0.0D0) THEN
                    FLCV(I,J,K)=FLCV(I,J,K)-FLC
                  ELSE
                    FLCV(I,J,K)=FLCV(I,J,K)-SU*WIN*CC(I,J  ,K)-SC*FLC
                  ENDIF
                ELSE
                  IF (WIN.LE.0.0D0) THEN
                    FLCV(I,J,K)=FLCV(I,J,K)-FLC
                  ELSE
                    FLCV(I,J,K)=FLCV(I,J,K)-SU*WIN*CC(I,J-1,K)-SC*FLC
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- (Gz*w)*Cの計算 --
      DO 320 K=2,NUMK
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (NF(I,J,K-1)*NF(I,J,K).EQ.0) THEN
              WIN=GGZ(I,J,K)*WW(I,J,K)
              IND=INDZ(I,J,K)

C             * 計算格子(通常処理)
              IF     (IND.EQ.0) THEN
                FLC=WIN*ZZ(6,K)*( ZZ(2,K-1)*CC(I,J,K  )
     &                           +ZZ(2,K  )*CC(I,J,K-1))
                FLCW(I,J,K)=FLCW(I,J,K)
     &                      -SU*( MAX( WIN,0.0D0)*CC(I,J,K-1)
     &                           -MAX(-WIN,0.0D0)*CC(I,J,K  ))
     &                      -SC*FLC

C             * 計算格子(特殊処理)
              ELSEIF (INDBC(IND).GE.1) THEN
                FLC=WIN*BCC(IND)
                IF (NF(I,J,K).EQ.0) THEN
                  IF (WIN.GE.0.0D0) THEN
                    FLCW(I,J,K)=FLCW(I,J,K)-FLC
                  ELSE
                    FLCW(I,J,K)=FLCW(I,J,K)-SU*WIN*CC(I,J,K  )-SC*FLC
                  ENDIF
                ELSE
                  IF (WIN.LE.0.0D0) THEN
                    FLCW(I,J,K)=FLCW(I,J,K)-FLC
                  ELSE
                    FLCW(I,J,K)=FLCW(I,J,K)-SU*WIN*CC(I,J,K-1)-SC*FLC
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
