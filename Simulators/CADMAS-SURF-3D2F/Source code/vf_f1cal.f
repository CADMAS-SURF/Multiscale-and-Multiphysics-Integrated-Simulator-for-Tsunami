      SUBROUTINE VF_F1CAL(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,ANU,
     &                    GGV,GGX,GGY,GGZ,
     &                    BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                    DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                    GGV0,DMTBTT,DMTBHH,DBUF,FLFU,FLFV,FLFW,QF,
     &                    WK05,WK06,WK07,WK08,WK09,WK10,WK11,
     &                    NF,INDX,INDY,INDZ,INDC,INDB,INDS,IBUF,NWK1
C----------------------------------------------------------2016.09 start
     &                    DELH,DELH_IN)
C----------------------------------------------------------2016.09 end

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
      INCLUDE 'SF_STRUCT.h'
C----------------------------------------------------------2016.09 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2016.09 end

CD    -- 引数 --
CD    XX(MAXG1,NUMI)      : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)      : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)      : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)        : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)        : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)        : I/O : R*8 : z方向流速
CD    PP(@FOR-3D@)        : I/O : R*8 : 圧力
CD    FF(@FOR-3D@)        : I/O : R*8 : VOF関数F
CD    FX(@FOR-3D@)        : I/O : R*8 : x方向スタッガードセルでのVOF関数Fx
CD    FY(@FOR-3D@)        : I/O : R*8 : y方向スタッガードセルでのVOF関数Fy
CD    FZ(@FOR-3D@)        : I/O : R*8 : z方向スタッガードセルでのVOF関数Fz
CD    ANU(@FOR-3D@)       : IN  : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGV(@FOR-3D@)       : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)       : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)       : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)       : IN  : R*8 : z方向面積透過率
CD    BCU(NUMB)           : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)           : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)           : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)           : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)           : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)          : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    TBUB(NUMK)          : I/O : R*8 : 気泡上昇処理を最後に行った時間
CD    DROPTX(@FOR-3D@)    : I/O : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@)    : I/O : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@)    : I/O : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@)    : I/O : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@)    : I/O : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@)    : I/O : R*8 : 自由落下のz方向速度
CD    GGV0(@FOR-3D@)      : IN  : R*8 : 空隙率(時間依存用)
CD    DMTBTT(MTBTT)       : IN  : R*8 : マトリクスデータの無次元位相
CD    DMTBHH(MTBTT)       : IN  : R*8 : マトリクスデータの水位
CD    DBUF(NUMBUF*MAXBUF) : OUT : R*8 : 並列用のバッファ
CD    FLFU(@FOR-3D@)      : OUT : R*8 : VOF関数Fのx方向フラックス
CD    FLFV(@FOR-3D@)      : OUT : R*8 : VOF関数Fのy方向フラックス
CD    FLFW(@FOR-3D@)      : OUT : R*8 : VOF関数Fのz方向フラックス
CD    QF(@FOR-3D@)        : OUT : R*8 : VOF関数Fの生成消滅
CD    WK05-WK11(@FOR-3D@) : --- : R*8 : ワーク
CD    NF(@FOR-3D@)        : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)      : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)      : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)      : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)      : I/O : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB)    : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)      : I/O : I*4 : 表面セルのI,J,K座標
CD    IBUF(NUMBUF*MAXBUF) : --- : I*4 : 並列用のバッファ
CD    NWK1(@FOR-3D@)      : --- : I*4 : 補正を行うためのワーク
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),FX  (NUMI,NUMJ,NUMK)
      DIMENSION FY  (NUMI,NUMJ,NUMK),FZ  (NUMI,NUMJ,NUMK)
      DIMENSION ANU (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION BCVI(NUMB),TBUB(NUMK)
      DIMENSION DROPTX(NUMI,NUMJ,NUMK),DROPTY(NUMI,NUMJ,NUMK)
      DIMENSION DROPTZ(NUMI,NUMJ,NUMK),DROPUU(NUMI,NUMJ,NUMK)
      DIMENSION DROPVV(NUMI,NUMJ,NUMK),DROPWW(NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK)
      DIMENSION DMTBTT(MTBTT),DMTBHH(MTBTT)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)
      DIMENSION FLFW(NUMI,NUMJ,NUMK),QF  (NUMI,NUMJ,NUMK)
      DIMENSION WK05(NUMI,NUMJ,NUMK),WK06(NUMI,NUMJ,NUMK)
      DIMENSION WK07(NUMI,NUMJ,NUMK),WK08(NUMI,NUMJ,NUMK)
      DIMENSION WK09(NUMI,NUMJ,NUMK),WK10(NUMI,NUMJ,NUMK)
      DIMENSION WK11(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
      DIMENSION INDS(NUMI*NUMJ*NUMK)
      DIMENSION IBUF(NUMBUF*MAXBUF) ,NWK1(NUMI,NUMJ,NUMK)
C----------------------------------------------------------2016.09 start
      DIMENSION DELH(NUMI0,NUMJ0),DELH_IN(NUMI0,NUMJ0)
C----------------------------------------------------------2016.09 end

C==== 実行 ===========================================================

CD    -- フラックスおよび生成消滅量のゼロクリア --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            FLFU(I,J,K)=0.0D0
            FLFV(I,J,K)=0.0D0
            FLFW(I,J,K)=0.0D0
            QF  (I,J,K)=0.0D0
CCC         NWK1(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 移流項によるフラックスの計算 --
      CALL VF_A2CPUT(0,ICPUST,KCPFFL)
      IF (ISCMFF.EQ.0) THEN
        CALL VF_FCONV (XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,BCF,
     &                 FLFU,FLFV,FLFW,NF,INDX,INDY,INDZ)
      ELSE
        CALL VF_FCONVS(XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,BCF,
     &                 DBUF,FLFU,FLFV,FLFW,
     &                 NF,INDX,INDY,INDZ,IBUF,NWK1,
     &                 WK05,WK06,WK07,WK08)
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPFFL)

CD    -- 生成消滅項の計算 --
CAKIY CALL FGENE(XX,ZZ,FF,WVU,QF,NF)

CD    -- 時間積分の計算 --
      CALL VF_A2CPUT(0,ICPUST,KCPFEL)
      IF (ICPL.GT.0 .OR. ISTM.EQ.1) THEN
        CALL VF_FEULER(XX,YY,ZZ,FF,GGV,GGV0,DBUF,FLFU,FLFV,FLFW,QF,NF)
      ELSEIF (IPRNT.LE.1) THEN
CSTR  IF (IPRNT.LE.1) THEN
        CALL VF_FEULER(XX,YY,ZZ,FF,GGV,GGV ,DBUF,FLFU,FLFV,FLFW,QF,NF)
      ELSE
C@@@        CALL VF_FEULER(XX,YY,ZZ,FF,GGV,GGV0,DBUF,FLFU,FLFV,FLFW,QF,NF)
        CALL VF_FEULER(XX,YY,ZZ,FF,GGV,GGV ,DBUF,FLFU,FLFV,FLFW,QF,NF)
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPFEL)
C----------------------------------------------------------2016.09 start
CD    -- 地形変化によるFの補正 --
      IF(ISEABT.NE.0) THEN
        CALL VF_FSEABT(XX,YY,ZZ,FF,GGV,DELH,DELH_IN,NF)
      ENDIF
C----------------------------------------------------------2016.09 end
CD    -- 水位固定時の境界面の処理 --
      CALL VF_BWFFSF(ZZ,FF,DMTBTT,DMTBHH,DBUF,NF,INDX,INDY,INDB)

CD    -- 表面セルに関する補正 --
      CALL VF_A2CPUT(0,ICPUST,KCPFMD)
      CALL VF_FMOD1(XX,YY,ZZ,FF,GGV,DBUF,NF,INDS,IBUF,NWK1)

CD    -- カットオフと空間積分の計算 --
      CALL VF_FCUT01(XX,YY,ZZ,FF,GGV,DBUF,NF)
      CALL VF_A2CPUT(0,ICPUEN,KCPFMD)

CD    -- 境界値の設定 --
      CALL VF_BWFF(FF,BCF,INDB)
C2F   CALL VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)
C2F   CALL VF_BSUWEM(UU,VV,WW,DBUF,NF,INDS,IBUF,NWK1)

CD    -- 更新前のNFをNWK1に退避 --
C2F      DO 520 K=1,NUMK
C2F        DO 510 J=1,NUMJ
C2F          DO 500 I=1,NUMI
C2F            NWK1(I,J,K)=NF(I,J,K)
C2F 500      CONTINUE
C2F 510    CONTINUE
C2F 520  CONTINUE

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
      CALL VF_FNFBUB(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF,NWK1)
      CALL VF_CINDC(NF,INDC)
      CALL VF_A2CPUT(0,ICPUEN,KCPFNF)

CD    -- x,y,z各方向のスタッガードセルにおけるF値を設定 --
      CALL VF_CFXYZ(XX,YY,ZZ,FF,FX,FY,FZ,GGV,DBUF,NF,INDX,INDY,INDZ)

CD    -- 気体セルから流体セルへ変化したセルの圧力の設定 --
C2F   CALL VF_BSPPFL(PP,FF,DBUF,WK05,NF,NWK1)

CD    -- 境界値の設定 --
C2F   CALL VF_BSUWT (XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDX,INDY,INDZ)
C2F   CALL VF_BSUWT2(UU,VV,WW,DBUF,NF,INDX,INDY,INDZ,INDS)
C2F   CALL VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)
C2F   CALL VF_BSPP  (XX,YY,ZZ,PP,FF,DBUF,NF)
C2F   CALL VF_BWUWT (XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCF,BCVI,INDB)
C2F   CALL VF_BWPP  (PP,BCP,INDB)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
