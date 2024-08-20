      SUBROUTINE VF_T1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                    TT,ALM,BCT,BCTI,DBUF,FLTU,FLTV,FLTW,QT,
     &                    NF,INDX,INDY,INDZ,INDB,INDS,INDBT)

CD=== 概要 ===========================================================

CDT   VF_T1CAL:温度の計算

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
CD    TT(@FOR-3D@)     : I/O : R*8 : 温度
CD    ALM(@FOR-3D@)    : IN  : R*8 : 熱伝導率と乱流熱伝導率の和
CD    BCT(NUMB)        : I/O : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : IN  : R*8 : 温度の境界条件
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLTU(@FOR-3D@)   : OUT : R*8 : VOF関数Fのx方向フラックス
CD    FLTV(@FOR-3D@)   : OUT : R*8 : VOF関数Fのy方向フラックス
CD    FLTW(@FOR-3D@)   : OUT : R*8 : VOF関数Fのz方向フラックス
CD    QT(@FOR-3D@)     : OUT : R*8 : VOF関数Fの生成消滅
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
CD    INDBT(NUMB)      : IN  : I*4 : 温度の境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK)
      DIMENSION TT  (NUMI,NUMJ,NUMK),ALM (NUMI,NUMJ,NUMK)
      DIMENSION BCT(NUMB),BCTI(2,NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION FLTU(NUMI,NUMJ,NUMK),FLTV(NUMI,NUMJ,NUMK)
      DIMENSION FLTW(NUMI,NUMJ,NUMK),QT  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)    ,INDS(NUMI*NUMJ*NUMK),INDBT(NUMB)

C==== 実行 ===========================================================

CD    -- フラックスおよび生成消滅量のゼロクリア --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            FLTU(I,J,K)=0.0D0
            FLTV(I,J,K)=0.0D0
            FLTW(I,J,K)=0.0D0
            QT  (I,J,K)=0.0D0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 移流項によるフラックスの計算 --
      IF (ISCMT.EQ.0) THEN
        CALL VF_SCONVD(0,XX,YY,ZZ,UU,VV,WW,TT,GGX,GGY,GGZ,BCT,
     &                 FLTU,FLTV,FLTW,NF,INDX,INDY,INDZ,INDBT)
      ELSE
        CALL VF_A2ERR('VF_T1CAL','DONOR ONLY.')
      ENDIF

CD    -- 拡散項によるフラックスの計算 --
      CALL VF_SDIFF(0,XX,YY,ZZ,TT,GGX,GGY,GGZ,BCT,ALM,
     &              FLTU,FLTV,FLTW,NF,INDX,INDY,INDZ)

CD    -- 生成消滅項の計算 --
CAKIY CALL VF_TGENE

CD    -- 時間積分の計算 --
      IF (IPRNT.LE.1) THEN
        CALL VF_SEULER(XX,YY,ZZ,TT,GGV,GGV ,DBUF,FLTU,FLTV,FLTW,QT,NF)
      ELSE
        CALL VF_SEULER(XX,YY,ZZ,TT,GGV,GGV0,DBUF,FLTU,FLTV,FLTW,QT,NF)
      ENDIF

CD    -- 境界値の設定 --
      CALL VF_BSSS(TT,DBUF,NF)
      CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,TT,ALM,BCT,BCTI,NF,INDB,INDBT)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
