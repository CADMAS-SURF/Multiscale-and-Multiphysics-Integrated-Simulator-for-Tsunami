      SUBROUTINE VF_V1CAL(ILOOP,XX,YY,ZZ,UU,VV,WW,PP,FF,ANU,CM0,CD0,
     &                    GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &                    BCU,BCV,BCW,BCP,BCVI,GGV0,GLV0,AK,TT,CC,
     &                    DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &                    DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,
     &                    DBUF,SRCUV,WK01,WK02,WK03,WK04,WK05,
     &                    WK06,WK07,WK08,WK09,WK10,
     &                    WK11,WK12,WK13,WK14,WK15,WK16,WK17,
     &                    NF,INDX,INDY,INDZ,INDC,INDB,INDS)

CD=== 概要 ===========================================================

CDT   VF_V1CAL:流速・圧力を計算する

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

CD    -- 引数 --
CD    ILOOP             : IN  : I*4 : サブループのステップ
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)      : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)      : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)      : I/O : R*8 : z方向流速
CD    PP(@FOR-3D@)      : I/O : R*8 : 圧力
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    ANU(@FOR-3D@)     : IN  : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    CM0(@FOR-3D@)     : IN  : R*8 : 慣性力係数
CD    CD0(@FOR-3D@)     : IN  : R*8 : 抵抗係数
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    GLV(@FOR-3D@)     : IN  : R*8 : =GGV+(1-GGV)*CM
CD    GLX(@FOR-3D@)     : IN  : R*8 : =GGX+(1-GGX)*CM
CD    GLY(@FOR-3D@)     : IN  : R*8 : =GGY+(1-GGY)*CM
CD    GLZ(@FOR-3D@)     : IN  : R*8 : =GGZ+(1-GGZ)*CM
CD    BCU(NUMB)         : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)         : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)         : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)         : I/O : R*8 : 圧力の境界値
CD    BCVI(NUMB)        : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    GGV0(@FOR-3D@)    : I/O : R*8 : 空隙率(時間依存用)
CD    GLV0(@FOR-3D@)    : I/O : R*8 : =GGV+(1-GGV)*CM(時間依存用)
CD    AK(@FOR-3D@)      : IN  : R*8 : 乱流エネルギ
CD    TT(@FOR-3D@)      : IN  : R*8 : 温度
CD    CC(@FOR-3D@,LEQC) : IN  : R*8 : 濃度
CD    DMTBTT(MTBTT)       : IN : R*8 : マトリクスデータの無次元位相
CD    DMTBZZ(MTBZZ)       : IN : R*8 : マトリクスデータのz座標
CD    DMTBHH(MTBTT)       : IN : R*8 : マトリクスデータの水位
CD    DMTBUN(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの鉛直方向流速
CD    DMTBTT2(MTBTT2)        : IN : R*8 : マトリクスデータ-2
CD    DMTBZZ2(MTBZZ2)        : IN : R*8 : マトリクスデータ-2
CD    DMTBHH2(MTBTT2)        : IN : R*8 : マトリクスデータ-2
CD    DMTBUN2(MTBZZ2,MTBTT2) : IN : R*8 : マトリクスデータ-2
CD    DMTBUT2(MTBZZ2,MTBTT2) : IN : R*8 : マトリクスデータ-2
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    SRCUV(NUMIJ,NUMK) : IN  : R*8 : 造波ソースのための流速
CD    WK01-17(@FOR-3D@) : OUT : R*8 : ワーク配列
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)    : IN  : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB)  : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)    : IN  : I*4 : 表面セルのI,J,K座標
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),ANU (NUMI,NUMJ,NUMK)
      DIMENSION CM0 (NUMI,NUMJ,NUMK),CD0 (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GLV (NUMI,NUMJ,NUMK),GLX (NUMI,NUMJ,NUMK)
      DIMENSION GLY (NUMI,NUMJ,NUMK),GLZ (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCVI(NUMB)
      DIMENSION GGV0(NUMI,NUMJ,NUMK),GLV0(NUMI,NUMJ,NUMK)
      DIMENSION AK  (NUMI,NUMJ,NUMK),TT  (NUMI,NUMJ,NUMK)
      DIMENSION CC  (NUMI,NUMJ,NUMK,LEQC)
      DIMENSION DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
      DIMENSION DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)
      DIMENSION DMTBTT2(MTBTT2),DMTBZZ2(MTBZZ2),DMTBHH2(MTBTT2)
      DIMENSION DMTBUN2(MTBZZ2,MTBTT2),DMTBUT2(MTBZZ2,MTBTT2)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION SRCUV(NUMIJ,NUMK)
      DIMENSION WK01(NUMI,NUMJ,NUMK),WK02(NUMI,NUMJ,NUMK)
      DIMENSION WK03(NUMI,NUMJ,NUMK),WK04(NUMI,NUMJ,NUMK)
      DIMENSION WK05(NUMI,NUMJ,NUMK),WK06(NUMI,NUMJ,NUMK)
      DIMENSION WK07(NUMI,NUMJ,NUMK),WK08(NUMI,NUMJ,NUMK)
      DIMENSION WK09(NUMI,NUMJ,NUMK),WK10(NUMI,NUMJ,NUMK)
      DIMENSION WK11(NUMI,NUMJ,NUMK),WK12(NUMI,NUMJ,NUMK)
      DIMENSION WK13(NUMI,NUMJ,NUMK),WK14(NUMI,NUMJ,NUMK)
      DIMENSION WK15(NUMI,NUMJ,NUMK),WK16(NUMI,NUMJ,NUMK)
      DIMENSION WK17(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
      DIMENSION INDS(NUMI*NUMJ*NUMK)

C==== 実行 ===========================================================

CD    -- サブループのステップが2以上ならば境界値の再設定 --
      IF (ILOOP.GE.2) THEN
        CALL VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)
        CALL VF_BWUWT (XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)
      ENDIF

CD    -- フラックスおよび生成消滅量のゼロクリア --
C     WK01=FLUU , WK02=FLUV , WK03=FLUW , WK04=QU
C     WK05=FLVU , WK06=FLVV , WK07=FLVW , WK08=QV
C     WK09=FLWU , WK10=FLWV , WK11=FLWW , WK12=QW
C     * 開境界のために前時刻の流速を格納する
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            WK01(I,J,K)=0.0D0
            WK02(I,J,K)=0.0D0
            WK03(I,J,K)=0.0D0
            WK04(I,J,K)=0.0D0
            WK05(I,J,K)=0.0D0
            WK06(I,J,K)=0.0D0
            WK07(I,J,K)=0.0D0
            WK08(I,J,K)=0.0D0
            WK09(I,J,K)=0.0D0
            WK10(I,J,K)=0.0D0
            WK11(I,J,K)=0.0D0
            WK12(I,J,K)=0.0D0
            WK13(I,J,K)=UU(I,J,K)
            WK14(I,J,K)=VV(I,J,K)
            WK15(I,J,K)=WW(I,J,K)
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- x方向流速の対流項および粘性項の計算 --
C     WK01=FLUU , WK02=FLUV , WK03=FLUW , WK04=QU
C     WK05=FLVU , WK06=FLVV , WK07=FLVW , WK08=QV
C     WK09=FLWU , WK10=FLWV , WK11=FLWW , WK12=QW
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      CALL VF_A2CPUT(0,ICPUST,KCPVFL)
      IF (ISCMVP.EQ.0) THEN
        CALL VF_VFLXDU(XX,YY,ZZ,UU,VV,WW,ANU,
     &                 GGX,GGY,GGZ,GLX,GLY,GLZ,BCU,
     &                 WK01,WK02,WK03,
     &                 NF,INDX,INDY,INDZ,INDC,INDB)
      ELSE
        CALL VF_A2ERR('VF_V1CAL','DONOR ONLY.')
      ENDIF

CD    -- y方向流速の対流項および粘性項の計算 --
C     WK01=FLUU , WK02=FLUV , WK03=FLUW , WK04=QU
C     WK05=FLVU , WK06=FLVV , WK07=FLVW , WK08=QV
C     WK09=FLWU , WK10=FLWV , WK11=FLWW , WK12=QW
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      IF (ISCMVP.EQ.0) THEN
        CALL VF_VFLXDV(XX,YY,ZZ,UU,VV,WW,ANU,
     &                 GGX,GGY,GGZ,GLX,GLY,GLZ,BCV,
     &                 WK05,WK06,WK07,
     &                 NF,INDX,INDY,INDZ,INDC,INDB)
      ELSE
        CALL VF_A2ERR('VF_V1CAL','DONOR ONLY.')
      ENDIF

CD    -- z方向流速の対流項および粘性項の計算 --
C     WK01=FLUU , WK02=FLUV , WK03=FLUW , WK04=QU
C     WK05=FLVU , WK06=FLVV , WK07=FLVW , WK08=QV
C     WK09=FLWU , WK10=FLWV , WK11=FLWW , WK12=QW
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      IF (ISCMVP.EQ.0) THEN
        CALL VF_VFLXDW(XX,YY,ZZ,UU,VV,WW,ANU,
     &                 GGX,GGY,GGZ,GLX,GLY,GLZ,BCW,
     &                 WK09,WK10,WK11,
     &                 NF,INDX,INDY,INDZ,INDC,INDB)
      ELSE
        CALL VF_A2ERR('VF_V1CAL','DONOR ONLY.')
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPVFL)

CD    -- 生成消滅項の計算 --
C     WK01=FLUU , WK02=FLUV , WK03=FLUW , WK04=QU
C     WK05=FLVU , WK06=FLVV , WK07=FLVW , WK08=QV
C     WK09=FLWU , WK10=FLWV , WK11=FLWW , WK12=QW
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      CALL VF_A2CPUT(0,ICPUST,KCPVGN)
      IF (IPRNT.LE.1) THEN
        CALL VF_VGENE(XX,YY,ZZ,UU,VV,WW,PP,
     &                CD0,GGV ,GGX,GGY,GGZ,AK,TT,CC,
     &                SRCUV,WK04,WK08,WK12,
     &                NF,INDX,INDY,INDZ)
      ELSE
        CALL VF_VGENE(XX,YY,ZZ,UU,VV,WW,PP,
     &                CD0,GGV0,GGX,GGY,GGZ,AK,TT,CC,
     &                SRCUV,WK04,WK08,WK12,
     &                NF,INDX,INDY,INDZ)
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPVGN)

CD    -- 仮流速の計算 --
C     WK01=FLUU , WK02=FLUV , WK03=FLUW , WK04=QU
C     WK05=FLVU , WK06=FLVV , WK07=FLVW , WK08=QV
C     WK09=FLWU , WK10=FLWV , WK11=FLWW , WK12=QW
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      CALL VF_A2CPUT(0,ICPUST,KCPVEL)
      IF (IPRNT.LE.1) THEN
        CALL VF_VEULER(XX,YY,ZZ,UU,VV,WW,GGV ,GLV,GLV ,DBUF,
     &                 WK01,WK02,WK03,WK04,
     &                 WK05,WK06,WK07,WK08,
     &                 WK09,WK10,WK11,WK12,
     &                 NF,INDX,INDY,INDZ)
      ELSE
        CALL VF_VEULER(XX,YY,ZZ,UU,VV,WW,GGV0,GLV,GLV0,DBUF,
     &                 WK01,WK02,WK03,WK04,
     &                 WK05,WK06,WK07,WK08,
     &                 WK09,WK10,WK11,WK12,
     &                 NF,INDX,INDY,INDZ)
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPVEL)

CD    -- 時間依存型の空隙率をスワップし、透過率を設定する --
      IF (IPRNT.GT.1) THEN
        CALL VF_CGGXYZ(1,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                 NF,INDX,INDY,INDZ)
        DO 220 K=1,NUMK
          DO 210 J=1,NUMJ
            DO 200 I=1,NUMI
              W          =GLV (I,J,K)
              GLV (I,J,K)=GLV0(I,J,K)
              GLV0(I,J,K)=W
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
        CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                 NF,INDX,INDY,INDZ)
      ENDIF

CD    -- 法線方向境界流速を設定 --
C     WK13=UUbk , WK14=VVbk , WK15=WWbk
      CALL VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,
     &              DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &              DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,DBUF,
     &              WK13,WK14,WK15,NF,INDX,INDY,INDB)

CD    -- ポテンシャル関数の連立1次方程式の作成 --
C     WK01=AD  , WK02=ALI , WK03=ALJ , WK04=ALK
C     WK05=AUI , WK06=AUJ , WK07=AUK , WK08=BB
      CALL VF_A2CPUT(0,ICPUST,KCPVPC)
      IF (IPRNT.LE.1) THEN
        CALL VF_VPCOEF(XX,YY,ZZ,UU,VV,WW,PP,FF,
     &                 GGV,GGX,GGY,GGZ,GLV,GGV ,DBUF,SRCUV,
     &                 WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08,
     &                 NF,INDX,INDY,INDZ,INDC,INDB)
      ELSE
        CALL VF_VPCOEF(XX,YY,ZZ,UU,VV,WW,PP,FF,
     &                 GGV,GGX,GGY,GGZ,GLV,GGV0,DBUF,SRCUV,
     &                 WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08,
     &                 NF,INDX,INDY,INDZ,INDC,INDB)
      ENDIF
      CALL VF_A2CPUT(0,ICPUEN,KCPVPC)

CD    -- ポテンシャル関数の連立1次方程式を解く --
C     WK01=AD  , WK02=ALI , WK03=ALJ , WK04=ALK
C     WK05=AUI , WK06=AUJ , WK07=AUK , WK08=BB
C     WK09=PT  , WK10-WK17=work
      CALL VF_A2CPUT(0,ICPUST,KCPVPS)
      CALL VF_VPSOL(DBUF,
     &              WK01,WK02,WK03,WK04,WK05,
     &              WK06,WK07,WK08,WK09,WK10,
     &              WK11,WK12,WK13,WK14,WK15,WK16,WK17,INDC)
      CALL VF_A2CPUT(0,ICPUEN,KCPVPS)

CD    -- 流速・圧力の補正 --
C     WK09=PT
      CALL VF_A2CPUT(0,ICPUST,KCPVMD)
      CALL VF_VMODIF(XX,YY,ZZ,UU,VV,WW,PP,GGV,GLV,
     &               BCU,BCV,BCW,DBUF,WK09,
     &               NF,INDX,INDY,INDZ,INDC,INDB)
      CALL VF_A2CPUT(0,ICPUEN,KCPVMD)

CD    -- 境界値の設定 --
      CALL VF_BSUWT (XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDX,INDY,INDZ)
      CALL VF_BSUWT2(UU,VV,WW,DBUF,NF,INDX,INDY,INDZ,INDS)
      CALL VF_BSUWN (XX,YY,ZZ,UU,VV,WW,GGX,GGY,GGZ,DBUF,SRCUV,NF,INDS)
      CALL VF_BWUWT (XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)
      CALL VF_BWPP  (PP,BCP,INDB)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
