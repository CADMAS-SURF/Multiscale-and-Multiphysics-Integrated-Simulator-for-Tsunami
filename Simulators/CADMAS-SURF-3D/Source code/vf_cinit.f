      SUBROUTINE VF_CINIT(XX ,YY ,ZZ ,
     &                    UU ,VV ,WW ,PP ,FF ,
     &                    ANU,GGV,GGX,GGY,GGZ,
     &                    BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                    DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                    ANUT,AK,AE,BCK,BCE,
     &                    TT,ALM,BCT,BCTI,CC,DD,BCC,BCCI,
     &                    DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &                    DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,
     &                    DBUF,PPPVC,WK01,WK02,WK03,
C----------------------------------------------------------2012.03 start
     &                    DELH,DELH_IN,
C----------------------------------------------------------2012.03 end
     &                    NF,INDX,INDY,INDZ,INDC,INDB,INDS,
     &                    INDBK,INDBE,INDBT,INDBC,IPVC,IBUF,NWK1)

CD=== 概要 ===========================================================

CDT   VF_CINIT:初期条件の設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ADBGI.h'
      INCLUDE 'VF_ADBGR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
C----------------------------------------------------------2012.03 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2012.03 end

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : OUT : R*8 : x方向流速
CD    VV(@FOR-3D@)     : OUT : R*8 : y方向流速
CD    WW(@FOR-3D@)     : OUT : R*8 : z方向流速
CD    PP(@FOR-3D@)     : OUT : R*8 : 圧力
CD    FF(@FOR-3D@)     : OUT : R*8 : VOF関数F
CD    ANU(@FOR-3D@)    : OUT : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : OUT : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)       : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    TBUB(NUMK)       : OUT : R*8 : 気泡上昇処理を最後に行った時間
CD    DROPTX(@FOR-3D@) : OUT : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@) : OUT : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@) : OUT : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@) : OUT : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@) : OUT : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@) : OUT : R*8 : 自由落下のz方向速度
CD    ANUT(@FOR-3D@)   : OUT : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)     : OUT : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : OUT : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB)        : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    TT(@FOR-3D@)     : OUT : R*8 : 温度
CD    ALM(@FOR-3D@)    : OUT : R*8 : 熱伝導率と乱流熱伝導率の和
CD    BCT(NUMB)        : I/O : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : IN  : R*8 : 温度の境界条件
CD    CC(@FOR-3D@,LEQC) : OUT : R*8 : 濃度
CD    DD(@FOR-3D@,LEQC) : OUT : R*8 : 拡散係数と乱流拡散係数の和
CD    BCC(NUMB,LEQC)    : I/O : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : IN  : R*8 : 濃度の境界条件
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
CD    PPPVC(@FOR-3D@)  : OUT : R*8 : 空気圧の計算用の圧力
CD    WK01-03(@FOR-3D@): OUT : R*8 : ワーク配列
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)   : OUT : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDS(@FOR-1D@)   : OUT : I*4 : 表面セルのI,J,K座標
CD    INDBK(NUMB)      : IN  : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : IN  : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : IN  : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : IN  : I*4 : 濃度の境界条件
CD    IPVC(@FOR-3D@)   : OUT : I*4 : 空気圧の計算用インデックス
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
CD    NWK1(@FOR-3D@)   : OUT : I*4 : ワーク
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
      DIMENSION ANUT(NUMI,NUMJ,NUMK),AK  (NUMI,NUMJ,NUMK)
      DIMENSION AE  (NUMI,NUMJ,NUMK),BCK(NUMB),BCE(NUMB)
      DIMENSION TT  (NUMI,NUMJ,NUMK),ALM (NUMI,NUMJ,NUMK)
      DIMENSION BCT(NUMB),BCTI(2,NUMB)
      DIMENSION CC(NUMI,NUMJ,NUMK,LEQC),DD(NUMI,NUMJ,NUMK,LEQC)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
      DIMENSION DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)
      DIMENSION DMTBTT2(MTBTT2),DMTBZZ2(MTBZZ2),DMTBHH2(MTBTT2)
      DIMENSION DMTBUN2(MTBZZ2,MTBTT2),DMTBUT2(MTBZZ2,MTBTT2)
      DIMENSION DBUF(NUMBUF*MAXBUF) ,PPPVC(NUMI,NUMJ,NUMK)
      DIMENSION WK01(NUMI,NUMJ,NUMK),WK02(NUMI,NUMJ,NUMK)
      DIMENSION WK03(NUMI,NUMJ,NUMK)
C----------------------------------------------------------2012.03 start
      DIMENSION DELH(NUMI0,NUMJ0),DELH_IN(NUMI0,NUMJ0)
C----------------------------------------------------------2012.03 end
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)    ,INDS(NUMI*NUMJ*NUMK)
      DIMENSION INDBK(NUMB),INDBE(NUMB),INDBT(NUMB),INDBC(NUMB,LEQC)
      DIMENSION IPVC(NUMI,NUMJ,NUMK),IBUF(NUMBUF*MAXBUF)
      DIMENSION NWK1(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 計算情報の初期値設定 --
      ICGITR=0
      CGBNRM=0.0D0
      CGXNRM=0.0D0
      FSUM  =0.0D0
      FCUT  =0.0D0

CD    -- VOF関数Fの初期値設定および境界条件設定 --
      CALL VF_ZSETR3(FF,0.0D0,NUMI,NUMJ,NUMK)
      DO 120 K=2,NUMK-1
        IF     (ZZ(1,K  ).GT.WVLVL) THEN
          VAL=0.0D0
        ELSEIF (ZZ(1,K+1).LT.WVLVL) THEN
          VAL=1.0D0
        ELSE
          VAL=(WVLVL-ZZ(1,K))/(ZZ(1,K+1)-ZZ(1,K))
        ENDIF
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (NF(I,J,K).NE.-1) FF(I,J,K)=VAL
cc            i1=i+mygis-1
cc            i1=i1*(mod(i1,2)*2-1)
cc            j1=j+mygjs-1
cc            j1=j1*(mod(j1,2)*2-1)
cc            IF (NF(I,J,K).NE.-1) 
cc     $         ff(i,j,k)=dble(i1)*1.d-2+dble(j1)*1.d-4
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE
      CALL VF_BWFFSF(ZZ,FF,DMTBTT,DMTBHH,DMTBTT2,DMTBHH2,DBUF,
     &               NF,INDX,INDY,INDB)
C     * デバッグ用の設定
      IF (IDBGF(1).GE.2) THEN
        DO 150 K=IDBGF(3),IDBGF(6)
          DO 140 J=IDBGF(2),IDBGF(5)
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 130 I=IDBGF(1),IDBGF(4)
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (NF(I-IP,J-JP,K).NE.-1) FF(I-IP,J-JP,K)=RDBGF
                ENDIF
 130          CONTINUE
            ENDIF
 140      CONTINUE
 150    CONTINUE
      ENDIF
C----------------------------------------------------------2012.03 start
      IF(ISEABT.NE.0) THEN
        CALL VF_FSEABT(XX,YY,ZZ,FF,GGV,DELH,DELH_IN,NF)
      END IF
C----------------------------------------------------------2012.03 end
      CALL VF_FCUT01(XX,YY,ZZ,FF,GGV,DBUF,NF)
      CALL VF_BWFF(ZZ,FF,BCF,INDX,INDY,INDB)

CD    -- NFの初期値設定 --
      CALL VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      CALL VF_FNFPRV(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)

CD    -- セルの計算状態を示すインデックスの初期設定 --
      CALL VF_ZSETI3(INDC,-1,NUMI,NUMJ,NUMK)
      CALL VF_CINDC(NF,INDC)

CD    -- 空気圧計算用インデックスの設定 --
      IF (PVCP0.GE.ZERO) THEN
        CALL VF_FPVCIP(NF,IPVC,IBUF,NWK1)
      ENDIF

CD    -- 流速の初期値設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=3
      IF (MYMIE.EQ.1) IB=NUMI-1
      IF (MYMJS.EQ.1) JA=3
      IF (MYMJE.EQ.1) JB=NUMJ-1
      CALL VF_ZSETR3(UU,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(VV,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WW,0.0D0,NUMI,NUMJ,NUMK)
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=IA,IB
            IF (INDX(I,J,K).EQ.0 .AND.
     &          NF(I-1,J,K)*NF(I,J,K).EQ.0) UU(I,J,K)=UINI
cc            i1=i+mygis-1
cc            i1=i1*(mod(i1,2)*2-1)
cc            j1=j+mygjs-1
cc            j1=-j1*(mod(j1,2)*2-1)
cc            uu(i,j,k)=dble(i1)*1.d-2+dble(j1)*1.d-4
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
      DO 250 K=2,NUMK-1
        DO 240 J=JA,JB
          DO 230 I=MYIS,MYIE
            IF (INDY(I,J,K).EQ.0 .AND.
     &          NF(I,J-1,K)*NF(I,J,K).EQ.0) VV(I,J,K)=VINI
cc            i1=i+mygis-1
cc            i1=i1*(mod(i1,2)*2-1)
cc            j1=j+mygjs-1
cc            j1=-j1*(mod(j1,2)*2-1)
cc            vv(i,j,k)=dble(i1)*1.d-2+dble(j1)*1.d-4
 230      CONTINUE
 240    CONTINUE
 250  CONTINUE
      DO 280 K=3,NUMK-1
        DO 270 J=MYJS,MYJE
          DO 260 I=MYIS,MYIE
            IF (INDZ(I,J,K).EQ.0 .AND.
     &          NF(I,J,K-1)*NF(I,J,K).EQ.0) WW(I,J,K)=WINI
cc            i1=i+mygis-1
cc            i1=i1*(mod(i1,2)*2-1)
cc            j1=j+mygjs-1
cc            j1=-j1*(mod(j1,2)*2-1)
cc            ww(i,j,k)=dble(i1)*1.d-2+dble(j1)*1.d-4
 260      CONTINUE
 270    CONTINUE
 280  CONTINUE
      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

CD    -- 圧力の初期値設定 --
      IA=1
      IB=NUMI
      JA=1
      JB=NUMJ
      IF (MYMIS.EQ.1) IA=2
      IF (MYMIE.EQ.1) IB=NUMI-1
      IF (MYMJS.EQ.1) JA=2
      IF (MYMJE.EQ.1) JB=NUMJ-1
      CALL VF_ZSETR3(PP,0.0D0,NUMI,NUMJ,NUMK)
      IF (PVCP0.GE.ZERO) THEN
        CALL VF_ZSETR3(PPPVC,1.0D30,NUMI,NUMJ,NUMK)
      ENDIF
      DO 320 K=2,NUMK-1
        VAL=RHO0*GRZ0*(WVLVL-(ZZ(1,K)+ZZ(1,K+1))*0.5D0)
        DO 310 J=JA,JB
          DO 300 I=IA,IB
            IF (INDC(I,J,K).NE.-1) PP(I,J,K)=VAL
            IF (PVCP0.GE.ZERO .AND. NF(I,J,K).GT.0) THEN
              PPPVC(I,J,K)=0.0D0
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

CD     -- 乱流エネルギと乱流エネルギ散逸の初期値設定 --
      IF (LEQK.NE.0) THEN
        IA=1
        IB=NUMI
        JA=1
        JB=NUMJ
        IF (MYMIS.EQ.1) IA=2
        IF (MYMIE.EQ.1) IB=NUMI-1
        IF (MYMJS.EQ.1) JA=2
        IF (MYMJE.EQ.1) JB=NUMJ-1
        CALL VF_ZSETR3(AK,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(AE,0.0D0,NUMI,NUMJ,NUMK)
        DO 420 K=2,NUMK-1
          DO 410 J=JA,JB
            DO 400 I=IA,IB
              IF (INDC(I,J,K).NE.-1) THEN
                AK(I,J,K)=AKINIK
                AE(I,J,K)=AKINIE
              ENDIF
 400        CONTINUE
 410      CONTINUE
 420    CONTINUE
      ENDIF

CD    -- 温度の初期値設定 --
      IF (LEQT.NE.0) THEN
        IA=1
        IB=NUMI
        JA=1
        JB=NUMJ
        IF (MYMIS.EQ.1) IA=2
        IF (MYMIE.EQ.1) IB=NUMI-1
        IF (MYMJS.EQ.1) JA=2
        IF (MYMJE.EQ.1) JB=NUMJ-1
        CALL VF_ZSETR3(TT,0.0D0,NUMI,NUMJ,NUMK)
        DO 520 K=2,NUMK-1
          DO 510 J=JA,JB
            DO 500 I=IA,IB
              IF (INDC(I,J,K).NE.-1) TT(I,J,K)=TINI
 500        CONTINUE
 510      CONTINUE
 520    CONTINUE
      ENDIF

CD    -- 濃度の初期値設定 --
      IA=1
      IB=NUMI
      JA=1
      JB=NUMJ
      IF (MYMIS.EQ.1) IA=2
      IF (MYMIE.EQ.1) IB=NUMI-1
      IF (MYMJS.EQ.1) JA=2
      IF (MYMJE.EQ.1) JB=NUMJ-1
      DO 630 LC=1,LEQC
        CALL VF_ZSETR3(CC(1,1,1,LC),0.0D0,NUMI,NUMJ,NUMK)
        DO 620 K=2,NUMK-1
          DO 610 J=JA,JB
            DO 600 I=IA,IB
              IF (INDC(I,J,K).NE.-1) CC(I,J,K,LC)=CINI(LC)
 600        CONTINUE
 610      CONTINUE
 620    CONTINUE
 630  CONTINUE

CD    -- 動粘性係数等の初期値設定 --
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETR3(ANUT,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_CNUT0(AK,AE,ANUT,NF)
      ENDIF
      CALL VF_ZSETR3(ANU,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_CNU00(ANUT,ANU,NF)
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETR3(ALM,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_CLM00(ANUT,ALM,NF)
      ENDIF
      IF (LEQC.GT.0) THEN
        DO 650 LC=1,LEQC
          CALL VF_ZSETR3(DD(1,1,1,LC),0.0D0,NUMI,NUMJ,NUMK)
 650    CONTINUE
        CALL VF_CDD00(ANUT,DD,NF)
      ENDIF

CD    -- その他の境界条件の設定 --
C     * 開境界のために前時刻の流速を仮定する
      DO 720 K=1,NUMK
        DO 710 J=1,NUMJ
          DO 700 I=1,NUMI
            WK01(I,J,K)=UU(I,J,K)
            WK02(I,J,K)=VV(I,J,K)
            WK03(I,J,K)=WW(I,J,K)
 700      CONTINUE
 710    CONTINUE
 720  CONTINUE
      CALL VF_BWUWN (XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,
     &               DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &               DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,DBUF,
     &               WK01,WK02,WK03,NF,INDX,INDY,INDB)
      CALL VF_BSUWT (XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDX,INDY,INDZ)
      CALL VF_BSUWT2(UU,VV,WW,DBUF,NF,INDX,INDY,INDZ,INDS)
      CALL VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)
      CALL VF_BSPP  (XX,YY,ZZ,PP,FF,DBUF,NF)
      CALL VF_BWUWT (XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)
      CALL VF_BWPP  (PP,BCP,INDB)
      IF (LEQK.NE.0) THEN
        CALL VF_BSSS(AK,DBUF,NF)
        CALL VF_BSSS(AE,DBUF,NF)
        CALL VF_BWKE(AK,AE,BCK,BCE,INDB,INDBK,INDBE)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_BSSS(TT,DBUF,NF)
        CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,TT,ALM,BCT,BCTI,NF,INDB,INDBT)
      ENDIF
      DO 730 LC=1,LEQC
        CALL VF_BSSS(CC(1,1,1,LC),DBUF,NF)
        CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,CC(1,1,1,LC),DD(1,1,1,LC),
     &               BCC(1,LC),BCCI(1,1,LC),NF,INDB,INDBC(1,LC))
 730  CONTINUE

CD    -- TimerDoor法のための設定 --
      DO 800 K=1,NUMK
        TBUB(K)=0.0D0
 800  CONTINUE
      DO 830 K=1,NUMK
        DO 820 J=1,NUMJ
          DO 810 I=1,NUMI
            DROPTX(I,J,K)=-1.0D10
            DROPTY(I,J,K)= 0.0D0
            DROPTZ(I,J,K)= 0.0D0
            DROPUU(I,J,K)= 0.0D0
            DROPVV(I,J,K)= 0.0D0
            DROPWW(I,J,K)= 0.0D0
 810      CONTINUE
 820    CONTINUE
 830  CONTINUE
C     * デバッグ用の設定
      IF (IDBGTD(1).GE.2) THEN
        DO 860 K=IDBGTD(3),IDBGTD(6)
          DO 850 J=IDBGTD(2),IDBGTD(5)
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 840 I=IDBGTD(1),IDBGTD(4)
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (NF(I-IP,J-JP,K).NE.-1) THEN
                    DROPTX(I-IP,J-JP,K)=0.0D0
                    DROPTY(I-IP,J-JP,K)=0.0D0
                    DROPTZ(I-IP,J-JP,K)=0.0D0
                    DROPUU(I-IP,J-JP,K)=RDBGTD(1)
                    DROPVV(I-IP,J-JP,K)=RDBGTD(2)
                    DROPWW(I-IP,J-JP,K)=RDBGTD(3)
                  ENDIF
                ENDIF
 840          CONTINUE
            ENDIF
 850      CONTINUE
 860    CONTINUE
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
