      SUBROUTINE VF_S1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                    CC,DD,BCC,BCCI,DBUF,FLCU,FLCV,FLCW,QC,
     &                    NF,INDX,INDY,INDZ,INDB,INDS,INDBC)

CD=== 概要 ===========================================================

CDT   VF_S1CAL:スカラ量(濃度)の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    GGV0(@FOR-3D@)   : IN  : R*8 : 空隙率(時間依存用)
CD    CC(@FOR-3D@,LEQC) : I/O : R*8 : 濃度
CD    DD(@FOR-3D@,LEQC) : IN  : R*8 : 拡散係数と乱流拡散係数の和
CD    BCC(NUMB,LEQC)    : I/O : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : IN  : R*8 : 濃度の境界条件
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLCU(@FOR-3D@)   : OUT : R*8 : x方向フラックス
CD    FLCV(@FOR-3D@)   : OUT : R*8 : y方向フラックス
CD    FLCW(@FOR-3D@)   : OUT : R*8 : z方向フラックス
CD    QC(@FOR-3D@)     : OUT : R*8 : 生成消滅
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
CD    INDBC(NUMB,LEQC) : IN  : I*4 : 境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK)
      DIMENSION CC(NUMI,NUMJ,NUMK,LEQC),DD(NUMI,NUMJ,NUMK,LEQC)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION FLCU(NUMI,NUMJ,NUMK),FLCV(NUMI,NUMJ,NUMK)
      DIMENSION FLCW(NUMI,NUMJ,NUMK),QC  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB),INDS(NUMI*NUMJ*NUMK),INDBC(NUMB,LEQC)

C==== 実行 ===========================================================

CD    -- 成分毎に濃度を更新 --
      DO 1000 LC=1,LEQC

CD      -- フラックスおよび生成消滅量のゼロクリア --
        DO 120 K=1,NUMK
          DO 110 J=1,NUMJ
            DO 100 I=1,NUMI
              FLCU(I,J,K)=0.0D0
              FLCV(I,J,K)=0.0D0
              FLCW(I,J,K)=0.0D0
              QC  (I,J,K)=0.0D0
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE

CD      -- 移流項によるフラックスの計算 --
        IF (ISCMC(LC).EQ.0) THEN
          CALL VF_SCONVD(LC,XX,YY,ZZ,UU,VV,WW,
     &                   CC(1,1,1,LC),GGX,GGY,GGZ,BCC(1,LC),
     &                   FLCU,FLCV,FLCW,NF,INDX,INDY,INDZ,INDBC(1,LC))
        ELSE
          CALL VF_A2ERR('VF_T1CAL','DONOR ONLY.')
        ENDIF

CD      -- 拡散項によるフラックスの計算 --
        CALL VF_SDIFF(LC,XX,YY,ZZ,CC(1,1,1,LC),
     &                GGX,GGY,GGZ,BCC(1,LC),DD(1,1,1,LC),
     &                FLCU,FLCV,FLCW,NF,INDX,INDY,INDZ)

CD      -- 生成消滅項の計算 --
CAKIY   CALL VF_SGENE

CD      -- 時間積分の計算 --
        IF (IPRNT.LE.1) THEN
          CALL VF_SEULER(XX,YY,ZZ,CC(1,1,1,LC),
     &                   GGV,GGV ,DBUF,FLCU,FLCV,FLCW,QC,NF)
        ELSE
          CALL VF_SEULER(XX,YY,ZZ,CC(1,1,1,LC),
     &                   GGV,GGV0,DBUF,FLCU,FLCV,FLCW,QC,NF)
        ENDIF

CD      -- 境界値の設定 --
        CALL VF_BSSS(CC(1,1,1,LC),DBUF,NF)
        CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,CC(1,1,1,LC),DD(1,1,1,LC),
     &               BCC(1,LC),BCCI(1,1,LC),NF,INDB,INDBC(1,LC))

 1000 CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
