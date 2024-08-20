      SUBROUTINE VF_JPCOEF(XX,YY,ZZ,PP,
     &                     AD,ALI,ALJ,ALK,AUI,AUJ,AUK,BB,PT,
     &                     NF,INDX,INDY,INDZ,INDC2)

CD=== 概要 ===========================================================

CDT   VF_JPCOEF:HiDEM用のポテンシャル関数の連立1次方程式を作成する
CD      (1)対角項を正にするため、Poisson式の係数には-1.0を乗じる

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    PP(@FOR-3D@)        : IN  : R*8 : 圧力
CD    AD(@FOR-3D@)      : OUT : R*8 : 非対称行列Aの対角成分
CD    ALI(@FOR-3D@)     : OUT : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(@FOR-3D@)     : OUT : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(@FOR-3D@)     : OUT : R*8 : 非対称行列AのK-1に関する成分
CD    AUI(@FOR-3D@)     : OUT : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(@FOR-3D@)     : OUT : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(@FOR-3D@)     : OUT : R*8 : 非対称行列AのK+1に関する成分
CD    BB(@FOR-3D@)      : OUT : R*8 : 非対称連立1次方程式の右辺
CD    PT(@FOR-3D@)      : OUT : R*8 : ポアッソン式の解（初期値を設定）
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
CD    INDC2(@FOR-3D@)   : OUT : I*4 : セルの計算状態を示すインデックス(HiDEMとの連成用)
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION PP  (NUMI,NUMJ,NUMK)
      DIMENSION AD  (NUMI,NUMJ,NUMK),ALI (NUMI,NUMJ,NUMK)
      DIMENSION ALJ (NUMI,NUMJ,NUMK),ALK (NUMI,NUMJ,NUMK)
      DIMENSION AUI (NUMI,NUMJ,NUMK),AUJ (NUMI,NUMJ,NUMK)
      DIMENSION AUK (NUMI,NUMJ,NUMK),BB  (NUMI,NUMJ,NUMK)
      DIMENSION PT  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC2(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- ワークのゼロクリアとINDC2の作成 --
      DO 30 K=1,NUMK
        DO 20 J=1,NUMJ
          DO 10 I=1,NUMI
            AD (I,J,K)=0.0D0
            ALI(I,J,K)=0.0D0
            ALJ(I,J,K)=0.0D0
            ALK(I,J,K)=0.0D0
            AUI(I,J,K)=0.0D0
            AUJ(I,J,K)=0.0D0
            AUK(I,J,K)=0.0D0
            BB (I,J,K)=0.0D0
            IF     (NF(I,J,K).EQ.-2) THEN
              INDC2(I,J,K)= 0
              PT   (I,J,K)= 0.0D0
            ELSEIF (NF(I,J,K).EQ.-1) THEN
              INDC2(I,J,K)=-1
              PT   (I,J,K)= 0.0D0
            ELSE
              INDC2(I,J,K)=-1
              PT   (I,J,K)= PP(I,J,K)
            ENDIF
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE

CD    -- 並列時の範囲変更 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=1
      IF (MYMJS.EQ.1) JA=1
      IF (MYMIE.EQ.1) IB=NUMI
      IF (MYMJE.EQ.1) JB=NUMJ

CD    -- 係数行列と右辺の作成 --
      DO 120 K=1,NUMK
        DO 110 J=JA,JB
          DO 100 I=IA,IB

CD          -- 非計算セル --
            IF (INDC2(I,J,K).EQ.-1) THEN
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=PT(I,J,K)

CD          -- 計算セル --
            ELSE

              IM =I-1
              IP =I+1
              JM =J-1
              JP =J+1
              KM =K-1
              KP =K+1
              AXY=XX(2,I)*YY(2,J)
              AXZ=XX(2,I)*ZZ(2,K)
              AYZ=YY(2,J)*ZZ(2,K)
              AVL=XX(2,I)*YY(2,J)*ZZ(2,K)

CD            -- ALI:x方向負側の面 --
              N2=NF(IM,J,K)
CD            * 移動障害物内部および流体との境界面
              IF (N2.EQ.-2 .OR. N2.GE.0) THEN
                AIM=AYZ*XX(5,I)
                ALI(I,J,K)=-AIM
              ELSE
                AIM=0.0D0
                ALI(I,J,K)=0.0D0
              ENDIF

CD            -- AUI:x方向正側の面 --
              N2=NF(IP,J,K)
CD            * 移動障害物内部および流体との境界面
              IF (N2.EQ.-2 .OR. N2.GE.0) THEN
                AIP=AYZ*XX(5,IP)
                AUI(I,J,K)=-AIP
              ELSE
                AIP=0.0D0
                AUI(I,J,K)=0.0D0
              ENDIF

CD            -- ALJ:y方向負側の面 --
              N2=NF(I,JM,K)
CD            * 移動障害物内部および流体との境界面
              IF (N2.EQ.-2 .OR. N2.GE.0) THEN
                AJM=AXZ*YY(5,J)
                ALJ(I,J,K)=-AJM
              ELSE
                AJM=0.0D0
                ALJ(I,J,K)=0.0D0
              ENDIF

CD            -- AUJ:y方向正側の面 --
              N2=NF(I,JP,K)
CD            * 移動障害物内部および流体との境界面
              IF (N2.EQ.-2 .OR. N2.GE.0) THEN
                AJP=AXZ*YY(5,JP)
                AUJ(I,J,K)=-AJP
              ELSE
                AJP=0.0D0
                AUJ(I,J,K)=0.0D0
              ENDIF

CD            -- ALK:z方向負側の面 --
              N2=NF(I,J,KM)
CD            * 移動障害物内部および流体との境界面
              IF (N2.EQ.-2 .OR. N2.GE.0) THEN
                AKM=AXY*ZZ(5,K)
                ALK(I,J,K)=-AKM
              ELSE
                AKM=0.0D0
                ALK(I,J,K)=0.0D0
              ENDIF

CD            -- AUK:z方向正側の面 --
              N2=NF(I,J,KP)
CD            * 移動障害物内部および流体との境界面
              IF (N2.EQ.-2 .OR. N2.GE.0) THEN
                AKP=AXY*ZZ(5,KP)
                AUK(I,J,K)=-AKP
              ELSE
                AKP=0.0D0
                AUK(I,J,K)=0.0D0
              ENDIF

CD            -- AD:対角項 --
              AD(I,J,K)=AKM+AJM+AIM+AIP+AJP+AKP

CD            -- BB:右辺 --
              BB(I,J,K)=0.0D0

            ENDIF

 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
