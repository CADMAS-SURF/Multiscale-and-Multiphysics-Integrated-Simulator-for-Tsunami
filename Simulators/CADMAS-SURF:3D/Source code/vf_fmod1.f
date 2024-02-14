      SUBROUTINE VF_FMOD1(XX,YY,ZZ,FF,GGV,DBUF,NF,INDS,IBUF,NLIM)

CD=== 概要 ===========================================================

CDT   VF_FMOD1:表面セルに関する補正(大域的)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
CD    NLIM(@FOR-3D@)   : IN  : I*4 : 補正を行うためのワーク
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),GGV (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDS(NUMI*NUMJ*NUMK)
      DIMENSION IBUF(NUMBUF*MAXBUF) ,NLIM(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- ゼロクリア --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            NLIM(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 流体・気体セルから見て自分を向いている表面セルの数を数える --
      DO 200 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        N  =NF(I,J,K)
        IEC=I
        JEC=J
        KEC=K
        IFC=I
        JFC=J
        KFC=K
        IF     (N.EQ.5) THEN
          KEC=K+1
          KFC=K-1
        ELSEIF (N.EQ.6) THEN
          KEC=K-1
          KFC=K+1
        ELSEIF (N.EQ.3) THEN
          JEC=J+1
          JFC=J-1
        ELSEIF (N.EQ.4) THEN
          JEC=J-1
          JFC=J+1
        ELSEIF (N.EQ.1) THEN
          IEC=I+1
          IFC=I-1
        ELSEIF (N.EQ.2) THEN
          IEC=I-1
          IFC=I+1
        ENDIF
        NLIM(IEC,JEC,KEC)=NLIM(IEC,JEC,KEC)+1
        NLIM(IFC,JFC,KFC)=NLIM(IFC,JFC,KFC)+1
 200  CONTINUE

      CALL VF_P3SRI1(NLIM,IBUF,0)

CD    -- 流体・表面・気体セルの3セル間で補正(NLIM=1) --
      DO 300 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        N  =NF(I,J,K)
        IEC=I
        JEC=J
        KEC=K
        IFC=I
        JFC=J
        KFC=K
        IF     (N.EQ.5) THEN
          KEC=K+1
          KFC=K-1
        ELSEIF (N.EQ.6) THEN
          KEC=K-1
          KFC=K+1
        ELSEIF (N.EQ.3) THEN
          JEC=J+1
          JFC=J-1
        ELSEIF (N.EQ.4) THEN
          JEC=J-1
          JFC=J+1
        ELSEIF (N.EQ.1) THEN
          IEC=I+1
          IFC=I-1
        ELSEIF (N.EQ.2) THEN
          IEC=I-1
          IFC=I+1
        ENDIF
C       * 3セルの有効体積
        VE=XX(2,IEC)*YY(2,JEC)*ZZ(2,KEC)*GGV(IEC,JEC,KEC)
        VS=XX(2,I  )*YY(2,J  )*ZZ(2,K  )*GGV(I  ,J  ,K  )
        VF=XX(2,IFC)*YY(2,JFC)*ZZ(2,KFC)*GGV(IFC,JFC,KFC)
C       * 3セルの水量を合計する
        SS=VS*FF(I,J,K)
        IF (NLIM(IEC,JEC,KEC).EQ.1) SS=SS+VE*FF(IEC,JEC,KEC)
        IF (NLIM(IFC,JFC,KFC).EQ.1) SS=SS+VF*FF(IFC,JFC,KFC)
C       * 流体・表面・気体セルの順で水を分配する
        IF (NLIM(IFC,JFC,KFC).EQ.1) THEN
          DS=MIN(VF,SS)
          SS=SS-DS
          FF(IFC,JFC,KFC)=DS/VF
        ENDIF
        DS=MIN(VS,SS)
        SS=SS-DS
        FF(I,J,K)=DS/VS
        IF (NLIM(IEC,JEC,KEC).EQ.1) THEN
          DS=MIN(VE,SS)
          SS=SS-DS
          FF(IEC,JEC,KEC)=DS/VE
        ENDIF
C       * 気体・表面セルの順で余った水を分配する(FF>1.0となる)
        IF (SS.GT.0.0D0) THEN
          IF (NLIM(IEC,JEC,KEC).EQ.1) THEN
            FF(IEC,JEC,KEC)=FF(IEC,JEC,KEC)+SS/VE
          ELSE
            FF(I  ,J  ,K  )=FF(I  ,J  ,K  )+SS/VS
          ENDIF
        ENDIF
 300  CONTINUE

      CALL VF_P3SRD1(FF,DBUF,0)

CD    -- 表面セルの入りすぎと出すぎを補正 --
      DO 400 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        N  =NF(I,J,K)
        IEC=I
        JEC=J
        KEC=K
        IFC=I
        JFC=J
        KFC=K
        IF     (N.EQ.5) THEN
          KEC=K+1
          KFC=K-1
        ELSEIF (N.EQ.6) THEN
          KEC=K-1
          KFC=K+1
        ELSEIF (N.EQ.3) THEN
          JEC=J+1
          JFC=J-1
        ELSEIF (N.EQ.4) THEN
          JEC=J-1
          JFC=J+1
        ELSEIF (N.EQ.1) THEN
          IEC=I+1
          IFC=I-1
        ELSEIF (N.EQ.2) THEN
          IEC=I-1
          IFC=I+1
        ENDIF
C       * 3セルの有効体積
        VE=XX(2,IEC)*YY(2,JEC)*ZZ(2,KEC)*GGV(IEC,JEC,KEC)
        VS=XX(2,I  )*YY(2,J  )*ZZ(2,K  )*GGV(I  ,J  ,K  )
        VF=XX(2,IFC)*YY(2,JFC)*ZZ(2,KFC)*GGV(IFC,JFC,KFC)
C       * 入りすぎた表面セルから気体セルへ
        IF (FF(I,J,K).GT.1.0D0) THEN
          FF(IEC,JEC,KEC)=FF(IEC,JEC,KEC)+VS*(FF(I,J,K)-1.0D0)/VE
          FF(I  ,J  ,K  )=1.0D0
        ENDIF
C       * 流体セルから出すぎた表面セルへ
        IF (FF(I,J,K).LT.0.0D0) THEN
          FF(IFC,JFC,KFC)=FF(IFC,JFC,KFC)+VS*FF(I,J,K)/VF
          FF(I  ,J  ,K  )=0.0D0
        ENDIF
 400  CONTINUE

      CALL VF_P3SRD2(FF,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
