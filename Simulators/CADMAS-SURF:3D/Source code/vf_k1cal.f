      SUBROUTINE VF_K1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                    BCU,BCV,BCW,BCVI,ANUT,AK,AE,BCK,BCE,DBUF,
     &                    FLKU,FLKV,FLKW,QK,FLEU,FLEV,FLEW,QE,WKNU,
     &                    NF,INDX,INDY,INDZ,INDB,INDS,INDBK,INDBE)

CD=== 概要 ===========================================================

CDT   VF_K1CAL:k-ε2方程式モデルの計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'

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
CD    BCU(NUMB)        : IN  : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : IN  : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : IN  : R*8 : z方向流速の境界値
CD    BCVI(NUMB)       : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    ANUT(@FOR-3D@)   : IN  : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)     : I/O : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : I/O : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB)        : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLKU(@FOR-3D@)   : OUT : R*8 : kのx方向フラックス
CD    FLKV(@FOR-3D@)   : OUT : R*8 : kのy方向フラックス
CD    FLKW(@FOR-3D@)   : OUT : R*8 : kのz方向フラックス
CD    QK(@FOR-3D@)     : OUT : R*8 : kの生成消滅
CD    FLEU(@FOR-3D@)   : OUT : R*8 : εのx方向フラックス
CD    FLEV(@FOR-3D@)   : OUT : R*8 : εのy方向フラックス
CD    FLEW(@FOR-3D@)   : OUT : R*8 : εのz方向フラックス
CD    QE(@FOR-3D@)     : OUT : R*8 : εの生成消滅
CD    WKNU(@FOR-3D@)   : OUT : R*8 : 作業用の動粘性係数
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
CD    INDBK(NUMB)      : IN  : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : IN  : I*4 : 乱流エネルギ散逸の境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCVI(NUMB)
      DIMENSION ANUT(NUMI,NUMJ,NUMK)
      DIMENSION AK  (NUMI,NUMJ,NUMK),AE  (NUMI,NUMJ,NUMK)
      DIMENSION BCK (NUMB)          ,BCE (NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION FLKU(NUMI,NUMJ,NUMK),FLKV(NUMI,NUMJ,NUMK)
      DIMENSION FLKW(NUMI,NUMJ,NUMK),QK  (NUMI,NUMJ,NUMK)
      DIMENSION FLEU(NUMI,NUMJ,NUMK),FLEV(NUMI,NUMJ,NUMK)
      DIMENSION FLEW(NUMI,NUMJ,NUMK),QE  (NUMI,NUMJ,NUMK)
      DIMENSION WKNU(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)    ,INDS(NUMI*NUMJ*NUMK)
      DIMENSION INDBK(NUMB)         ,INDBE(NUMB)

C==== 実行 ===========================================================

CD    -- フラックスおよび生成消滅量のゼロクリア --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            FLKU(I,J,K)=0.0D0
            FLKV(I,J,K)=0.0D0
            FLKW(I,J,K)=0.0D0
            QK  (I,J,K)=0.0D0
            FLEU(I,J,K)=0.0D0
            FLEV(I,J,K)=0.0D0
            FLEW(I,J,K)=0.0D0
            QE  (I,J,K)=0.0D0
            WKNU(I,J,K)=0.0D0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 移流項によるフラックスの計算 --
      IF (ISCMK.EQ.0) THEN
        CALL VF_SCONVD(-1,XX,YY,ZZ,UU,VV,WW,AK,GGX,GGY,GGZ,BCK,
     &                 FLKU,FLKV,FLKW,NF,INDX,INDY,INDZ,INDBK)
        CALL VF_SCONVD(-1,XX,YY,ZZ,UU,VV,WW,AE,GGX,GGY,GGZ,BCE,
     &                 FLEU,FLEV,FLEW,NF,INDX,INDY,INDZ,INDBE)
      ELSE
        CALL VF_A2ERR('VF_K1CAL','DONOR ONLY.')
      ENDIF

CD    -- 拡散項によるフラックスの計算 --
      W=1.0D0/AKSGK
      DO 220 K=2,NUMK-1
        DO 210 J=2,NUMJ-1
          DO 200 I=2,NUMI-1
            IF (NF(I,J,K).NE.-1) WKNU(I,J,K)=ANU0+ANUT(I,J,K)*W
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
      CALL VF_SDIFF(-1,XX,YY,ZZ,AK,GGX,GGY,GGZ,BCK,WKNU,
     &              FLKU,FLKV,FLKW,NF,INDX,INDY,INDZ)
      W=1.0D0/AKSGE
      DO 320 K=2,NUMK-1
        DO 310 J=2,NUMJ-1
          DO 300 I=2,NUMI-1
            IF (NF(I,J,K).NE.-1) WKNU(I,J,K)=ANU0+ANUT(I,J,K)*W
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE
      CALL VF_SDIFF(-1,XX,YY,ZZ,AE,GGX,GGY,GGZ,BCE,WKNU,
     &              FLEU,FLEV,FLEW,NF,INDX,INDY,INDZ)

CD    -- 生成消滅項の計算 --
      CALL VF_KGENE(XX,YY,ZZ,UU,VV,WW,GGV,BCU,BCV,BCW,
     &              ANUT,AK,AE,QK,QE,NF,INDX,INDY,INDZ)

CD    -- 時間積分の計算 --
      IF (IPRNT.LE.1) THEN
        CALL VF_SEULER(XX,YY,ZZ,AK,GGV,GGV ,DBUF,FLKU,FLKV,FLKW,QK,NF)
        CALL VF_SEULER(XX,YY,ZZ,AE,GGV,GGV ,DBUF,FLEU,FLEV,FLEW,QE,NF)
      ELSE
        CALL VF_SEULER(XX,YY,ZZ,AK,GGV,GGV0,DBUF,FLKU,FLKV,FLKW,QK,NF)
        CALL VF_SEULER(XX,YY,ZZ,AE,GGV,GGV0,DBUF,FLEU,FLEV,FLEW,QE,NF)
      ENDIF

CD    -- 対数則境界面に接するセル中心の乱流量を設定 --
      CALL VF_BWKELG(XX,YY,ZZ,UU,VV,WW,BCVI,AK,AE,DBUF,
     &               NF,INDX,INDY,INDZ,INDB)

CD    -- カットオフ --
      DO 420 K=2,NUMK-1
        DO 410 J=MYJS,MYJE
          DO 400 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.0) THEN
              IF     (AE(I,J,K).LT.AKMINE) THEN
                AK(I,J,K)=AKMINK
                AE(I,J,K)=AKMINE
              ELSEIF (AK(I,J,K).LT.AKMINK) THEN
                AK(I,J,K)=AKMINK
              ENDIF
            ENDIF
 400      CONTINUE
 410    CONTINUE
 420  CONTINUE

CD    -- 境界値の設定 --
      CALL VF_BSSS(AK,DBUF,NF)
      CALL VF_BSSS(AE,DBUF,NF)
      CALL VF_BWKE(AK,AE,BCK,BCE,INDB,INDBK,INDBE)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
