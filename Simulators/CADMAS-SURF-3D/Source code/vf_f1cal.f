      SUBROUTINE VF_F1CAL(XX,YY,ZZ,UU,VV,WW,PP,FF,ANU,GGV,GGX,GGY,GGZ,
     &                    BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                    DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                    GGV0,DMTBTT,DMTBHH,DMTBTT2,DMTBHH2,
     &                    DBUF,SRCUV,PPPVC,
     &                    FLFU,FLFV,FLFW,QF,
     &                    WK05,WK06,WK07,WK08,WK09,WK10,WK11,
     &                    NF,INDX,INDY,INDZ,INDC,INDB,INDS,
     &                    IPVC,IBUF,NLIM,
C----------------------------------------------------------2012.03 start
C    &                    FFLXX,FFLXY)
     &                    FFLXX,FFLXY,DELH,DELH_IN)
C----------------------------------------------------------2012.03 end

CD=== 概要 ===========================================================

CDT   VF_F1CAL:VOF関数Fの計算およびNFの設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'
C----------------------------------------------------------2012.03 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2012.03 end

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : I/O : R*8 : z方向流速
CD    PP(@FOR-3D@)     : I/O : R*8 : 圧力
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    ANU(@FOR-3D@)    : IN  : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)       : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    TBUB(NUMK)       : I/O : R*8 : 気泡上昇処理を最後に行った時間
CD    DROPTX(@FOR-3D@) : I/O : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@) : I/O : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@) : I/O : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@) : I/O : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@) : I/O : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@) : I/O : R*8 : 自由落下のz方向速度
CD    GGV0(@FOR-3D@)   : IN  : R*8 : 空隙率(時間依存用)
CD    DMTBTT(MTBTT)    : IN  : R*8 : マトリクスデータの無次元位相
CD    DMTBHH(MTBTT)    : IN  : R*8 : マトリクスデータの水位
CD    DMTBTT2(MTBTT2)  : IN  : R*8 : マトリクスデータ-2
CD    DMTBHH2(MTBTT2)  : IN  : R*8 : マトリクスデータ-2
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    SRCUV(NUMIJ,NUMK) : IN  : R*8 : 造波ソースのための流速
CD    PPPVC(@FOR-3D@)  : I/O : R*8 : 空気圧の計算用の圧力
CD    FLFU(@FOR-3D@)   : OUT : R*8 : VOF関数Fのx方向フラックス
CD    FLFV(@FOR-3D@)   : OUT : R*8 : VOF関数Fのy方向フラックス
CD    FLFW(@FOR-3D@)   : OUT : R*8 : VOF関数Fのz方向フラックス
CD    QF(@FOR-3D@)     : OUT : R*8 : VOF関数Fの生成消滅
CD    WK05-WK11(@FOR-3D@) : OUT : R*8 : ワーク
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)   : I/O : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)   : I/O : I*4 : 表面セルのI,J,K座標
CD    IPVC(@FOR-3D@)   : OUT : I*4 : 空気圧の計算用インデックス
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
CD    NLIM(@FOR-3D@)   : OUT : I*4 : 補正を行うためのワーク
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),ANU (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION BCVI(NUMB),TBUB(NUMK)
      DIMENSION DROPTX(NUMI,NUMJ,NUMK),DROPTY(NUMI,NUMJ,NUMK)
      DIMENSION DROPTZ(NUMI,NUMJ,NUMK),DROPUU(NUMI,NUMJ,NUMK)
      DIMENSION DROPVV(NUMI,NUMJ,NUMK),DROPWW(NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK)
      DIMENSION DMTBTT (MTBTT ),DMTBHH (MTBTT )
      DIMENSION DMTBTT2(MTBTT2),DMTBHH2(MTBTT2)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION SRCUV(NUMIJ,NUMK),  PPPVC(NUMI,NUMJ,NUMK)
      DIMENSION FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)
      DIMENSION FLFW(NUMI,NUMJ,NUMK),QF  (NUMI,NUMJ,NUMK)
      DIMENSION WK05(NUMI,NUMJ,NUMK),WK06(NUMI,NUMJ,NUMK)
      DIMENSION WK07(NUMI,NUMJ,NUMK),WK08(NUMI,NUMJ,NUMK)
      DIMENSION WK09(NUMI,NUMJ,NUMK),WK10(NUMI,NUMJ,NUMK)
      DIMENSION WK11(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
      DIMENSION INDS(NUMI*NUMJ*NUMK),IPVC(NUMI,NUMJ,NUMK)
      DIMENSION IBUF(NUMBUF*MAXBUF) ,NLIM(NUMI,NUMJ,NUMK)
      DIMENSION FFLXX(NUMI,NUMJ,NUMK),FFLXY(NUMI,NUMJ,NUMK)
C----------------------------------------------------------2012.03 start
      DIMENSION DELH(NUMI0,NUMJ0),DELH_IN(NUMI0,NUMJ0)
C----------------------------------------------------------2012.03 end

C==== 実行 ===========================================================

CD    -- フラックスおよび生成消滅量のゼロクリア --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            FLFU(I,J,K)=0.0D0
            FLFV(I,J,K)=0.0D0
            FLFW(I,J,K)=0.0D0
            QF  (I,J,K)=0.0D0
            NLIM(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 移流項によるフラックスの計算 --
      CALL VF_A2CPUT(0,ICPUST,KCPFFL)
      IF (ISCMFF.EQ.0) THEN
        CALL VF_FCONV (XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,BCF,
     &                 FLFU,FLFV,FLFW,NF,INDX,INDY,INDZ,NLIM)
      ELSE
        CALL VF_FCONVS(XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,BCF,
     &                 DBUF,FLFU,FLFV,FLFW,
     &                 NF,INDX,INDY,INDZ,IBUF,NLIM,
     &                 WK05,WK06,WK07,WK08)
      ENDIF
C     --- STOC(F) ---
      IF (NB_SC.GT.0) THEN
        CALL VF_STOC_FCONV(XX,YY,UU,VV,FF,GGV,GGX,GGY,BCF,
     &                     FLFU,FLFV,NF,INDX,INDY)
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPFFL)

CD    -- 生成消滅項の計算 --
      CALL VF_FGENE(XX,YY,FF,SRCUV,QF,NF)

CD    -- 時間積分の計算 --
      CALL VF_A2CPUT(0,ICPUST,KCPFEL)
      IF (IPRNT.LE.1) THEN
        CALL VF_FEULER(XX,YY,ZZ,FF,GGV,GGV ,DBUF,FLFU,FLFV,FLFW,QF,NF)
      ELSE
        CALL VF_FEULER(XX,YY,ZZ,FF,GGV,GGV0,DBUF,FLFU,FLFV,FLFW,QF,NF)
      ENDIF
C----------------------------------------------------------2012.03 start
CD    -- 地形変化によるFの補正 --
      IF(ISEABT.NE.0) THEN
        CALL VF_FSEABT(XX,YY,ZZ,FF,GGV,DELH,DELH_IN,NF)
      ENDIF

C----------------------------------------------------------2012.03 end
      DO 220 K=1,NUMK
        DO 210 J=1,NUMJ
          DO 200 I=1,NUMI
            FFLXX(I,J,K)=FLFU(I,J,K)
            FFLXY(I,J,K)=FLFV(I,J,K)
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
      CALL VF_A2CPUT(0,ICPUEN,KCPFEL)
      CALL VF_BWFFSF(ZZ,FF,DMTBTT,DMTBHH,DMTBTT2,DMTBHH2,DBUF,
     &               NF,INDX,INDY,INDB)

CD    -- 表面セルに関する補正 --
      CALL VF_A2CPUT(0,ICPUST,KCPFMD)
      CALL VF_FMOD1(XX,YY,ZZ,FF,GGV,DBUF,NF,INDS,IBUF,NLIM)

CD    -- カットオフと空間積分の計算 --
      CALL VF_FCUT01(XX,YY,ZZ,FF,GGV,DBUF,NF)
      CALL VF_A2CPUT(0,ICPUEN,KCPFMD)

CD    -- 境界値の設定 --
      CALL VF_BWFF(ZZ,FF,BCF,INDX,INDY,INDB)
      CALL VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)
      CALL VF_BSUWEM(UU,VV,WW,DBUF,NF,INDS,IBUF,NLIM)

CD    -- 更新前のNFをNLIMに退避 --
      DO 520 K=1,NUMK
        DO 510 J=1,NUMJ
          DO 500 I=1,NUMI
            NLIM(I,J,K)=NF(I,J,K)
 500      CONTINUE
 510    CONTINUE
 520  CONTINUE

CD    -- NFおよびINDCの設定 --
      CALL VF_A2CPUT(0,ICPUST,KCPFNF)
      CALL VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      IF (WBUB.GE.ZERO) THEN
        CALL VF_FBUBUP(ZZ,FF,GGV,TBUB,DBUF,NF)
        CALL VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      ENDIF
      IF (IDROP.GE.1) THEN
        CALL VF_FDROPF(XX,YY,ZZ,UU,VV,WW,FF,GGV,
     &                 DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                 DBUF,WK05,WK06,WK07,WK08,WK09,WK10,WK11,NF)
        CALL VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      ENDIF
      CALL VF_FNFPRV(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      CALL VF_CINDC(NF,INDC)
      CALL VF_A2CPUT(0,ICPUEN,KCPFNF)

CD    -- 空気圧 --
      IF (PVCP0.GE.ZERO) THEN
C       空気圧を計算する
        CALL VF_FPVCIP(NF,IPVC,IBUF,NLIM)
        CALL VF_FPVCPP(XX,YY,ZZ,UU,VV,WW,PP,FF,GGV,GGX,GGY,GGZ,
     &                 DBUF,PPPVC,NF,INDX,INDY,INDZ,INDB,IPVC)
      ELSE
C       気体セルから流体セルへ変化したセルの圧力の設定
        CALL VF_BSPPFL(PP,FF,DBUF,WK05,NF,NLIM)
      ENDIF

CD    -- 境界値の設定 --
      CALL VF_BSUWT (XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDX,INDY,INDZ)
      CALL VF_BSUWT2(UU,VV,WW,DBUF,NF,INDX,INDY,INDZ,INDS)
      CALL VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)
      CALL VF_BSPP  (XX,YY,ZZ,PP,FF,DBUF,NF)
      CALL VF_BWUWT (XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)
      CALL VF_BWPP  (PP,BCP,INDB)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
