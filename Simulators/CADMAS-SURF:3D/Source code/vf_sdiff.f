      SUBROUTINE VF_SDIFF(ISW,XX,YY,ZZ,CC,GGX,GGY,GGZ,BCC,DD,
     &                    FLCU,FLCV,FLCW,NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_SDIFF:スカラ量の拡散項によるフラックスの計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    ISW              : IN  : I*4 : 計算する物理量のスイッチ
CD                                   =-1:乱流量
CD                                   = 0:温度
CD                                   >=1:濃度(成分番号)
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    CC(@FOR-3D@)     : IN  : R*8 : スカラ量
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    BCC(NUMB)        : IN  : R*8 : 境界値
CD    DD(@FOR-3D@)     : IN  : R*8 : 拡散係数(熱伝導率等)
CD    FLCU(@FOR-3D@)   : I/O : R*8 : x方向フラックス
CD    FLCV(@FOR-3D@)   : I/O : R*8 : y方向フラックス
CD    FLCW(@FOR-3D@)   : I/O : R*8 : z方向フラックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION CC  (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION BCC (NUMB)          ,DD  (NUMI,NUMJ,NUMK)
      DIMENSION FLCU(NUMI,NUMJ,NUMK),FLCV(NUMI,NUMJ,NUMK)
      DIMENSION FLCW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 初期設定 --
      IF     (ISW.EQ.-1) THEN
        AL=1.0D0
      ELSEIF (ISW.EQ. 0) THEN
        AL=1.0D0/(RHO0*TCP0)
      ELSEIF (ISW.GE. 1) THEN
        AL=1.0D0
      ELSE
        CALL VF_A2ERR('VF_SDIFF','P.G ERROR.')
      ENDIF

CD    -- Gx*DD*(dC/dx)の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE+1
            IF (NF(I-1,J,K)*NF(I,J,K).EQ.0) THEN
              IND=INDX(I,J,K)

C             * 計算格子(通常処理)
              IF (IND.EQ.0) THEN
                D=XX(6,I)*(XX(2,I-1)*DD(I,J,K)+XX(2,I)*DD(I-1,J,K))
                D=D*AL
                FLCU(I,J,K)=FLCU(I,J,K)
     &                +GGX(I,J,K)*D*XX(5,I)*(CC(I,J,K)-CC(I-1,J,K))

C             * 計算格子(特殊処理)
              ELSE
                CW=BCC(IND)
                IF (NF(I,J,K).EQ.0) THEN
                  D=DD(I  ,J,K)*AL
                  FLCU(I,J,K)=FLCU(I,J,K)
     &                +GGX(I,J,K)*D*XX(4,I  )*(CC(I  ,J,K)-CW)*2.0D0
                ELSE
                  D=DD(I-1,J,K)*AL
                  FLCU(I,J,K)=FLCU(I,J,K)
     &                +GGX(I,J,K)*D*XX(4,I-1)*(CW-CC(I-1,J,K))*2.0D0
                ENDIF
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- Gy*DD*(dC/dy)の計算 --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE+1
          DO 200 I=MYIS,MYIE
            IF (NF(I,J-1,K)*NF(I,J,K).EQ.0) THEN
              IND=INDY(I,J,K)

C             * 計算格子(通常処理)
              IF (IND.EQ.0) THEN
                D=YY(6,J)*(YY(2,J-1)*DD(I,J,K)+YY(2,J)*DD(I,J-1,K))
                D=D*AL
                FLCV(I,J,K)=FLCV(I,J,K)
     &                +GGY(I,J,K)*D*YY(5,J)*(CC(I,J,K)-CC(I,J-1,K))

C             * 計算格子(特殊処理)
              ELSE
                CW=BCC(IND)
                IF (NF(I,J,K).EQ.0) THEN
                  D=DD(I,J  ,K)*AL
                  FLCV(I,J,K)=FLCV(I,J,K)
     &                +GGY(I,J,K)*D*YY(4,J  )*(CC(I,J  ,K)-CW)*2.0D0
                ELSE
                  D=DD(I,J-1,K)*AL
                  FLCV(I,J,K)=FLCV(I,J,K)
     &                +GGY(I,J,K)*D*YY(4,J-1)*(CW-CC(I,J-1,K))*2.0D0
                ENDIF
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- Gz*DD*(dC/dz)の計算 --
      DO 320 K=2,NUMK
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (NF(I,J,K-1)*NF(I,J,K).EQ.0) THEN
              IND=INDZ(I,J,K)

C             * 計算格子(通常処理)
              IF (IND.EQ.0) THEN
                D=ZZ(6,K)*(ZZ(2,K-1)*DD(I,J,K)+ZZ(2,K)*DD(I,J,K-1))
                D=D*AL
                FLCW(I,J,K)=FLCW(I,J,K)
     &                +GGZ(I,J,K)*D*ZZ(5,K)*(CC(I,J,K)-CC(I,J,K-1))

C             * 計算格子(特殊処理)
              ELSE
                CW=BCC(IND)
                IF (NF(I,J,K).EQ.0) THEN
                  D=DD(I,J,K  )*AL
                  FLCW(I,J,K)=FLCW(I,J,K)
     &                +GGZ(I,J,K)*D*ZZ(4,K  )*(CC(I,J,K  )-CW)*2.0D0
                ELSE
                  D=DD(I,J,K-1)*AL
                  FLCW(I,J,K)=FLCW(I,J,K)
     &                +GGZ(I,J,K)*D*ZZ(4,K-1)*(CW-CC(I,J,K-1))*2.0D0
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
