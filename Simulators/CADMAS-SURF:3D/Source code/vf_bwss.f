      SUBROUTINE VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,SS,DC,BCS,BCSI,
     &                   NF,INDB,INDBS)

CD=== 概要 ===========================================================

CDT   VF_BWSS:境界面のスカラ量を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    SS(@FOR-3D@)     : IN  : R*8 : スカラ量
CD    DC(@FOR-3D@)     : IN  : R*8 : 拡散係数と乱流拡散係数の和
CD    BCS(NUMB)        : I/O : R*8 : 境界値
CD    BCSI(2,NUMB)     : IN  : R*8 : 境界条件
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDBS(NUMB)      : IN  : I*4 : 境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
      DIMENSION GGZ(NUMI,NUMJ,NUMK)
      DIMENSION SS (NUMI,NUMJ,NUMK),DC(NUMI,NUMJ,NUMK)
      DIMENSION BCS(NUMB),BCSI(2,NUMB)
      DIMENSION NF (NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB),INDBS(NUMB)

C==== 実行 ===========================================================

CD    -- 境界面のスカラ量を設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        IB =ABS(INDBS(L))
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

CD      -- 計算セル側の値をとる --
        IF     (NS.EQ.1) THEN
          DL=XX(2,I  )*0.5D0
          SC=SS(I  ,J,K)
          GL=DC(I  ,J,K)*GGX(I,J,K)
          NC=NF(I  ,J,K)
        ELSEIF (NS.EQ.2) THEN
          DL=XX(2,I-1)*0.5D0
          SC=SS(I-1,J,K)
          GL=DC(I-1,J,K)*GGX(I,J,K)
          NC=NF(I-1,J,K)
        ELSEIF (NS.EQ.3) THEN
          DL=YY(2,J  )*0.5D0
          SC=SS(I,J  ,K)
          GL=DC(I,J  ,K)*GGY(I,J,K)
          NC=NF(I,J  ,K)
        ELSEIF (NS.EQ.4) THEN
          DL=YY(2,J-1)*0.5D0
          SC=SS(I,J-1,K)
          GL=DC(I,J-1,K)*GGY(I,J,K)
          NC=NF(I,J-1,K)
        ELSEIF (NS.EQ.5) THEN
          DL=ZZ(2,K  )*0.5D0
          SC=SS(I,J,K  )
          GL=DC(I,J,K  )*GGZ(I,J,K)
          NC=NF(I,J,K  )
        ELSEIF (NS.EQ.6) THEN
          DL=ZZ(2,K-1)*0.5D0
          SC=SS(I,J,K-1)
          GL=DC(I,J,K-1)*GGZ(I,J,K)
          NC=NF(I,J,K-1)
        ENDIF

CD      -- 濃度固定(温度固定) --
        IF     (IB.EQ.1) THEN
          BCS(L)=BCS(L)

CD      -- 勾配ゼロ(断熱) --
        ELSEIF (IB.EQ.2) THEN
          BCS(L)=SC

CD      -- 拡散流束(熱流束) --
        ELSEIF (IB.EQ.3) THEN
          IF (NC.EQ.8) THEN
            BCS(L)=0.0D0
          ELSE
            H=GL/DL
            BCS(L)=SC+BCSI(1,L)/H
          ENDIF

CD      -- 物質移動(熱伝達) --
        ELSEIF (IB.EQ.4) THEN
          IF (NC.EQ.8) THEN
            BCS(L)=0.0D0
          ELSE
            H=GL/DL
            BCS(L)=(H*SC+BCSI(1,L)*BCSI(2,L))/(H+BCSI(1,L))
          ENDIF

CD      -- プログラムエラー --
        ELSE
          CALL VF_A2ERR('VF_BWSS','P.G ERROR.')
        ENDIF

 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
