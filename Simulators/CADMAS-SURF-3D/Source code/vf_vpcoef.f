      SUBROUTINE VF_VPCOEF(XX,YY,ZZ,UU,VV,WW,PP,FF,
     &                     GGV,GGX,GGY,GGZ,GLV,GGV0,DBUF,SRCUV,
     &                     AD,ALI,ALJ,ALK,AUI,AUJ,AUK,BB,
     &                     NF,INDX,INDY,INDZ,INDC,INDB)

CD=== 概要 ===========================================================

CDT   VF_VPCOEF:ポテンシャル関数の連立1次方程式を作成する
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
CD    UU(@FOR-3D@)      : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)      : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)      : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)      : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    GLV(@FOR-3D@)     : IN  : R*8 : =GGV+(1-GGV)*CM
CD    GGV0(@FOR-3D@)    : IN  : R*8 : 空隙率(時間依存用)
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    SRCUV(NUMIJ,NUMK) : IN  : R*8 : 造波ソースのための流速
CD    AD(@FOR-3D@)      : OUT : R*8 : 非対称行列Aの対角成分
CD    ALI(@FOR-3D@)     : OUT : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(@FOR-3D@)     : OUT : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(@FOR-3D@)     : OUT : R*8 : 非対称行列AのK-1に関する成分
CD    AUI(@FOR-3D@)     : OUT : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(@FOR-3D@)     : OUT : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(@FOR-3D@)     : OUT : R*8 : 非対称行列AのK+1に関する成分
CD    BB(@FOR-3D@)      : OUT : R*8 : 非対称連立1次方程式の右辺
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)    : IN  : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB)  : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GLV (NUMI,NUMJ,NUMK),GGV0(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION SRCUV(NUMIJ,NUMK)
      DIMENSION AD  (NUMI,NUMJ,NUMK),ALI (NUMI,NUMJ,NUMK)
      DIMENSION ALJ (NUMI,NUMJ,NUMK),ALK (NUMI,NUMJ,NUMK)
      DIMENSION AUI (NUMI,NUMJ,NUMK),AUJ (NUMI,NUMJ,NUMK)
      DIMENSION AUK (NUMI,NUMJ,NUMK),BB  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)    

C==== 実行 ===========================================================

C@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE
C@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

CD    -- 並列時の範囲変更 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=1
      IF (MYMJS.EQ.1) JA=1
      IF (MYMIE.EQ.1) IB=NUMI
      IF (MYMJE.EQ.1) JB=NUMJ

CD    -- 造波ソースのための定数 --
      IWS=0
      JWS=0
      IF     (ISCTYP(1).GT.0) THEN
        IWS= ISCTYP(1)-(MYGIS-1)
      ELSEIF (ISCTYP(1).LT.0) THEN
        JWS=-ISCTYP(1)-(MYGJS-1)
      ENDIF

CD    -- 定数 --
      DTI=1.0D0/DTNOW
      TR =DTNOW/RHO0
      PPS=0.0D0

CD    -- 係数行列と右辺の作成 --
      DO 120 K=1,NUMK
        DO 110 J=JA,JB
          DO 100 I=IA,IB
            N=NF(I,J,K)

CD          -- 非計算セル --
            IF (INDC(I,J,K).EQ.-1) THEN
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=0.0D0

CD          -- 流体セル --
            ELSEIF (N.EQ.0) THEN

CD            -- 定数の計算 --
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
              IS=INDX(I,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=XX(6,I)*( XX(2,I )*GGV(I ,J,K)/GLV(I ,J,K)
     &                      +XX(2,IM)*GGV(IM,J,K)/GLV(IM,J,K))
                AIM=AYZ*XX(5,I)*GGX(I,J,K)*GV
                ALI(I,J,K)=-AIM
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
                IF     (INDB(3,IS).EQ.4) THEN
                  AIM=2.0D0*AYZ*XX(4,I)
     &                *GGX(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                  ALI(I,J,K)=0.0D0
                ELSEIF (INDB(3,IS).EQ.5) THEN
                  IF     (I.EQ.2 .AND. IBCTYP(2,1).EQ.-3
     &                           .AND. MTBTYP     .EQ. 3) THEN
                    AIM=2.0D0*AYZ*XX(4,I)
     &                  *GGX(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                    ALI(I,J,K)=0.0D0
                  ELSEIF (I.EQ.2 .AND. IBCTYP(2,1).EQ.-4
     &                           .AND. MTBTYP2    .EQ. 3) THEN
                    AIM=2.0D0*AYZ*XX(4,I)
     &                  *GGX(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                    ALI(I,J,K)=0.0D0
                  ELSE
                    AIM=0.0D0
                    ALI(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AIM=0.0D0
                  ALI(I,J,K)=0.0D0
                ENDIF
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AUI:x方向正側の面 --
              IS=INDX(IP,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=XX(6,IP)*( XX(2,IP)*GGV(IP,J,K)/GLV(IP,J,K)
     &                       +XX(2,I )*GGV(I ,J,K)/GLV(I ,J,K))
                AIP=AYZ*XX(5,IP)*GGX(IP,J,K)*GV
                AUI(I,J,K)=-AIP
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
                IF     (INDB(3,IS).EQ.4) THEN
                  AIP=2.0D0*AYZ*XX(4,I)
     &                *GGX(IP,J,K)*GGV(I,J,K)/GLV(I,J,K)
                  AUI(I,J,K)=0.0D0
                ELSEIF (INDB(3,IS).EQ.5) THEN
                  IF     (IP.EQ.NUMI .AND. IBCTYP(2,2).EQ.-3
     &                               .AND. MTBTYP     .EQ. 3) THEN
                    AIP=2.0D0*AYZ*XX(4,I)
     &                  *GGX(IP,J,K)*GGV(I,J,K)/GLV(I,J,K)
                    AUI(I,J,K)=0.0D0
                  ELSEIF (IP.EQ.NUMI .AND. IBCTYP(2,2).EQ.-4
     &                               .AND. MTBTYP2    .EQ. 3) THEN
                    AIP=2.0D0*AYZ*XX(4,I)
     &                  *GGX(IP,J,K)*GGV(I,J,K)/GLV(I,J,K)
                    AUI(I,J,K)=0.0D0
                  ELSE
                    AIP=0.0D0
                    AUI(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AIP=0.0D0
                  AUI(I,J,K)=0.0D0
                ENDIF
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- ALJ:y方向負側の面 --
              IS=INDY(I,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=YY(6,J)*( YY(2,J )*GGV(I,J ,K)/GLV(I,J ,K)
     &                      +YY(2,JM)*GGV(I,JM,K)/GLV(I,JM,K))
                AJM=AXZ*YY(5,J)*GGY(I,J,K)*GV
                ALJ(I,J,K)=-AJM
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
                IF     (INDB(3,IS).EQ.4) THEN
                  AJM=2.0D0*AXZ*YY(4,J)
     &                *GGY(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                  ALJ(I,J,K)=0.0D0
                ELSEIF (INDB(3,IS).EQ.5) THEN
                  IF     (J.EQ.2 .AND. IBCTYP(2,3).EQ.-3
     &                           .AND. MTBTYP     .EQ. 3) THEN
                    AJM=2.0D0*AXZ*YY(4,J)
     &                  *GGY(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                    ALJ(I,J,K)=0.0D0
                  ELSEIF (J.EQ.2 .AND. IBCTYP(2,3).EQ.-4
     &                           .AND. MTBTYP2    .EQ. 3) THEN
                    AJM=2.0D0*AXZ*YY(4,J)
     &                  *GGY(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                    ALJ(I,J,K)=0.0D0
                  ELSE
                    AJM=0.0D0
                    ALJ(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AJM=0.0D0
                  ALJ(I,J,K)=0.0D0
                ENDIF
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AUJ:y方向正側の面 --
              IS=INDY(I,JP,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=YY(6,JP)*( YY(2,JP)*GGV(I,JP,K)/GLV(I,JP,K)
     &                       +YY(2,J )*GGV(I,J ,K)/GLV(I,J ,K))
                AJP=AXZ*YY(5,JP)*GGY(I,JP,K)*GV
                AUJ(I,J,K)=-AJP
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
                IF     (INDB(3,IS).EQ.4) THEN
                  AJP=2.0D0*AXZ*YY(4,J)
     &                *GGY(I,JP,K)*GGV(I,J,K)/GLV(I,J,K)
                  AUJ(I,J,K)=0.0D0
                ELSEIF (INDB(3,IS).EQ.5) THEN
                  IF     (JP.EQ.NUMJ .AND. IBCTYP(2,4).EQ.-3
     &                               .AND. MTBTYP     .EQ. 3) THEN
                    AJP=2.0D0*AXZ*YY(4,J)
     &                  *GGY(I,JP,K)*GGV(I,J,K)/GLV(I,J,K)
                    AUJ(I,J,K)=0.0D0
                  ELSEIF (JP.EQ.NUMJ .AND. IBCTYP(2,4).EQ.-4
     &                               .AND. MTBTYP2    .EQ. 3) THEN
                    AJP=2.0D0*AXZ*YY(4,J)
     &                  *GGY(I,JP,K)*GGV(I,J,K)/GLV(I,J,K)
                    AUJ(I,J,K)=0.0D0

                  ELSE
                    AJP=0.0D0
                    AUJ(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AJP=0.0D0
                  AUJ(I,J,K)=0.0D0
                ENDIF
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- ALK:z方向負側の面 --
              IS=INDZ(I,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=ZZ(6,K)*( ZZ(2,K )*GGV(I,J,K )/GLV(I,J,K )
     &                      +ZZ(2,KM)*GGV(I,J,KM)/GLV(I,J,KM))
                AKM=AXY*ZZ(5,K)*GGZ(I,J,K)*GV
                ALK(I,J,K)=-AKM
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
                IF (INDB(3,IS).NE.4) THEN
                  AKM=0.0D0
                  ALK(I,J,K)=0.0D0
                ELSE
                  AKM=2.0D0*AXY*ZZ(4,K)
     &                *GGZ(I,J,K)*GGV(I,J,K)/GLV(I,J,K)
                  ALK(I,J,K)=0.0D0
                ENDIF
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AUK:z方向正側の面 --
              IS=INDZ(I,J,KP)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=ZZ(6,KP)*( ZZ(2,KP)*GGV(I,J,KP)/GLV(I,J,KP)
     &                       +ZZ(2,K )*GGV(I,J,K )/GLV(I,J,K ))
                AKP=AXY*ZZ(5,KP)*GGZ(I,J,KP)*GV
                AUK(I,J,K)=-AKP
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
                IF (INDB(3,IS).NE.4) THEN
                  AKP=0.0D0
                  AUK(I,J,K)=0.0D0
                ELSE
                  AKP=2.0D0*AXY*ZZ(4,K)
     &                *GGZ(I,J,KP)*GGV(I,J,K)/GLV(I,J,K)
                  AUK(I,J,K)=0.0D0
                ENDIF
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AD:対角項 --
              AD(I,J,K)=AKM+AJM+AIM+AIP+AJP+AKP

CD            -- BB:右辺 --
              VD= AYZ*(GGX(IP,J,K)*UU(IP,J,K)-GGX(I,J,K)*UU(I,J,K))
     &           +AXZ*(GGY(I,JP,K)*VV(I,JP,K)-GGY(I,J,K)*VV(I,J,K))
     &           +AXY*(GGZ(I,J,KP)*WW(I,J,KP)-GGZ(I,J,K)*WW(I,J,K))
              VQ=-(GGV(I,J,K)-GGV0(I,J,K))*DTI*AVL
              IF (IWS.EQ.I) VQ=2.0D0*ZZ(2,K)*SRCUV(J,K)*YY(2,J)
              IF (JWS.EQ.J) VQ=2.0D0*ZZ(2,K)*SRCUV(I,K)*XX(2,I)
              BB(I,J,K)=VD-VQ

CD          -- 表面セル:z負方向に流体 --
            ELSEIF (N.EQ.5) THEN
              IF (PVCP0.GE.ZERO) PPS=PP(I,J,K+1)
              PSI=ZZ(3,K  )/(0.5D0*ZZ(2,K-1)+FF(I,J,K)*ZZ(2,K))
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=PSI-1.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=-TR*((1.0D0-PSI)*PP(I,J,K-1)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:z正方向に流体 --
            ELSEIF (N.EQ.6) THEN
              IF (PVCP0.GE.ZERO) PPS=PP(I,J,K-1)
              PSI=ZZ(3,K+1)/(0.5D0*ZZ(2,K+1)+FF(I,J,K)*ZZ(2,K))
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=PSI-1.0D0
              BB (I,J,K)=-TR*((1.0D0-PSI)*PP(I,J,K+1)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:y負方向に流体 --
            ELSEIF (N.EQ.3) THEN
              IF (PVCP0.GE.ZERO) PPS=PP(I,J+1,K)
              PSI=YY(3,J  )/(0.5D0*YY(2,J-1)+FF(I,J,K)*YY(2,J))
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=PSI-1.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=-TR*((1.0D0-PSI)*PP(I,J-1,K)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:y正方向に流体 --
            ELSEIF (N.EQ.4) THEN
              IF (PVCP0.GE.ZERO) PPS=PP(I,J-1,K)
              PSI=YY(3,J+1)/(0.5D0*YY(2,J+1)+FF(I,J,K)*YY(2,J))
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=PSI-1.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=-TR*((1.0D0-PSI)*PP(I,J+1,K)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:x負方向に流体 --
            ELSEIF (N.EQ.1) THEN
              IF (PVCP0.GE.ZERO) PPS=PP(I+1,J,K)
              PSI=XX(3,I  )/(0.5D0*XX(2,I-1)+FF(I,J,K)*XX(2,I))
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=PSI-1.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=0.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=-TR*((1.0D0-PSI)*PP(I-1,J,K)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:x正方向に流体 --
            ELSEIF (N.EQ.2) THEN
              IF (PVCP0.GE.ZERO) PPS=PP(I-1,J,K)
              PSI=XX(3,I+1)/(0.5D0*XX(2,I+1)+FF(I,J,K)*XX(2,I))
              AD (I,J,K)=1.0D0
              ALI(I,J,K)=0.0D0
              ALJ(I,J,K)=0.0D0
              ALK(I,J,K)=0.0D0
              AUI(I,J,K)=PSI-1.0D0
              AUJ(I,J,K)=0.0D0
              AUK(I,J,K)=0.0D0
              BB (I,J,K)=-TR*((1.0D0-PSI)*PP(I+1,J,K)+PSI*PPS-PP(I,J,K))

CD          -- プログラムエラー --
            ELSE
              CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

C@      CALL VF_P3SRD1(AD ,DBUF,0)
C@      CALL VF_P3SRD1(ALI,DBUF,0)
C@      CALL VF_P3SRD1(ALJ,DBUF,0)
C@      CALL VF_P3SRD1(ALK,DBUF,0)
C@      CALL VF_P3SRD1(AUI,DBUF,0)
C@      CALL VF_P3SRD1(AUJ,DBUF,0)
C@      CALL VF_P3SRD1(AUK,DBUF,0)
C@      CALL VF_P3SRD1(BB ,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
