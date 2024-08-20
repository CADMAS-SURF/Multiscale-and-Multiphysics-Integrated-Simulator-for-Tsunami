      SUBROUTINE VF_VPCOEF(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,BCF,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                     RHOG,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                     GGV,GGX,GGY,GGZ,GLV,GGV0,DBUF,
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
      INCLUDE 'SF_STRUCT.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)      : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)      : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)      : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)      : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    FX(@FOR-3D@)      : IN  : R*8 : x方向スタッガードセルでのVOF関数Fx
CD    FY(@FOR-3D@)      : IN  : R*8 : y方向スタッガードセルでのVOF関数Fy
CD    FZ(@FOR-3D@)      : IN  : R*8 : z方向スタッガードセルでのVOF関数Fz
CD    BCF(NUMB)         : IN  : R*8 : VOF関数Fの境界値
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    GLV(@FOR-3D@)     : IN  : R*8 : =GGV+(1-GGV)*CM
CD    GGV0(@FOR-3D@)    : IN  : R*8 : 空隙率(時間依存用)
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
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
      DIMENSION FF  (NUMI,NUMJ,NUMK),FX  (NUMI,NUMJ,NUMK)
      DIMENSION FY  (NUMI,NUMJ,NUMK),FZ  (NUMI,NUMJ,NUMK)
      DIMENSION BCF (NUMB)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION RHOG(NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GLV (NUMI,NUMJ,NUMK),GGV0(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION AD  (NUMI,NUMJ,NUMK),ALI (NUMI,NUMJ,NUMK)
      DIMENSION ALJ (NUMI,NUMJ,NUMK),ALK (NUMI,NUMJ,NUMK)
      DIMENSION AUI (NUMI,NUMJ,NUMK),AUJ (NUMI,NUMJ,NUMK)
      DIMENSION AUK (NUMI,NUMJ,NUMK),BB  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)    

      DIMENSION TR(2)
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

CAKIY -- 造波ソースのための定数 --

CD    -- 定数 --
      DTI=1.0D0/DTNOW
      PPS=0.0D0
C@@@@
      DVQQQ=0.0D0
C@@@@

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
C2F         ELSEIF (N.EQ.0) THEN
            ELSEIF (N.GT.-1) THEN
              IF (N.NE.8) THEN
                LGBF=3
              ELSE
                LGBF=5
              ENDIF

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FX(I,J,K)*RHO0(1)+(1.0D0-FX(I,J,K))*RHO0(2))
                RHO02 = XX(6,I)*( XX(2,I )*RHOG(I ,J,K)
     &                           +XX(2,IM)*RHOG(IM,J,K))
                RI=1.0D0/(FX(I,J,K)*RHO0(1)+(1.0D0-FX(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                AIM=AYZ*XX(5,I)*GGX(I,J,K)*GV*RI
                ALI(I,J,K)=-AIM
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
                RHO02 = RHOG(I,J,K)
                RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                IF     (INDB(LGBF,IS).EQ.4) THEN
                  AIM=2.0D0*AYZ*XX(4,I)
     &                *GGX(I,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                  ALI(I,J,K)=0.0D0
                ELSEIF (INDB(LGBF,IS).EQ.5) THEN
                  IF (I .EQ.2      .AND. IBCTYP(2,1).EQ.-3
     &                             .AND. MTBTYP     .EQ. 3) THEN
                    AIM=2.0D0*AYZ*XX(4,I)
     &                  *GGX(I,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                    ALI(I,J,K)=0.0D0
                  ELSE
                    AIM=0.0D0
                    ALI(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AIM=0.0D0
                  ALI(I,J,K)=0.0D0
                ENDIF
              ELSEIF(ICPL.GT.0) THEN
                AIM=0.0D0
                ALI(I,J,K)=0.0D0
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AUI:x方向正側の面 --
              IS=INDX(IP,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=XX(6,IP)*( XX(2,IP)*GGV(IP,J,K)/GLV(IP,J,K)
     &                       +XX(2,I )*GGV(I ,J,K)/GLV(I ,J,K))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FX(IP,J,K)*RHO0(1)+(1.0D0-FX(IP,J,K))*RHO0(2))
                RHO02 = XX(6,IP)*( XX(2,IP)*RHOG(IP,J,K)
     &                            +XX(2,I )*RHOG(I ,J,K))
                RI=1.0D0/(FX(IP,J,K)*RHO0(1)+(1.0D0-FX(IP,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                AIP=AYZ*XX(5,IP)*GGX(IP,J,K)*GV*RI
                AUI(I,J,K)=-AIP
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
                RHO02 = RHOG(I,J,K)
                RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                IF     (INDB(LGBF,IS).EQ.4) THEN
                  AIP=2.0D0*AYZ*XX(4,I)
     &                *GGX(IP,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                  AUI(I,J,K)=0.0D0
                ELSEIF (INDB(LGBF,IS).EQ.5) THEN
                  IF (IP.EQ.NUMI   .AND. IBCTYP(2,2).EQ.-3
     &                             .AND. MTBTYP     .EQ. 3) THEN
                    AIP=2.0D0*AYZ*XX(4,I)
     &                  *GGX(IP,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                    AUI(I,J,K)=0.0D0
                  ELSE
                    AIP=0.0D0
                    AUI(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AIP=0.0D0
                  AUI(I,J,K)=0.0D0
                ENDIF
              ELSEIF(ICPL.GT.0) THEN
                AIP=0.0D0
                AUI(I,J,K)=0.0D0
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- ALJ:y方向負側の面 --
              IS=INDY(I,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=YY(6,J)*( YY(2,J )*GGV(I,J ,K)/GLV(I,J ,K)
     &                      +YY(2,JM)*GGV(I,JM,K)/GLV(I,JM,K))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FY(I,J,K)*RHO0(1)+(1.0D0-FY(I,J,K))*RHO0(2))
                RHO02 = YY(6,J)*( YY(2,J )*RHOG(I,J ,K)
     &                           +YY(2,JM)*RHOG(I,JM,K))
                RI=1.0D0/(FY(I,J,K)*RHO0(1)+(1.0D0-FY(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                AJM=AXZ*YY(5,J)*GGY(I,J,K)*GV*RI
                ALJ(I,J,K)=-AJM
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
                RHO02 = RHOG(I,J,K)
                RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                IF     (INDB(LGBF,IS).EQ.4) THEN
                  AJM=2.0D0*AXZ*YY(4,J)
     &                *GGY(I,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                  ALJ(I,J,K)=0.0D0
                ELSEIF (INDB(LGBF,IS).EQ.5) THEN
                  IF (J .EQ.2      .AND. IBCTYP(2,3).EQ.-3
     &                             .AND. MTBTYP     .EQ. 3) THEN
                    AJM=2.0D0*AXZ*YY(4,J)
     &                  *GGY(I,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                    ALJ(I,J,K)=0.0D0
                  ELSE
                    AJM=0.0D0
                    ALJ(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AJM=0.0D0
                  ALJ(I,J,K)=0.0D0
                ENDIF
              ELSEIF(ICPL.GT.0) THEN
                AJM=0.0D0
                ALJ(I,J,K)=0.0D0
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AUJ:y方向正側の面 --
              IS=INDY(I,JP,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=YY(6,JP)*( YY(2,JP)*GGV(I,JP,K)/GLV(I,JP,K)
     &                       +YY(2,J )*GGV(I,J ,K)/GLV(I,J ,K))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FY(I,JP,K)*RHO0(1)+(1.0D0-FY(I,JP,K))*RHO0(2))
                RHO02 = YY(6,JP)*( YY(2,JP)*RHOG(I,JP,K)
     &                            +YY(2,J )*RHOG(I,J ,K))
                RI=1.0D0/(FY(I,JP,K)*RHO0(1)+(1.0D0-FY(I,JP,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                AJP=AXZ*YY(5,JP)*GGY(I,JP,K)*GV*RI
                AUJ(I,J,K)=-AJP
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
                RHO02 = RHOG(I,J,K)
                RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                IF     (INDB(LGBF,IS).EQ.4) THEN
                  AJP=2.0D0*AXZ*YY(4,J)
     &                *GGY(I,JP,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                  AUJ(I,J,K)=0.0D0
                ELSEIF (INDB(LGBF,IS).EQ.5) THEN
                  IF (JP.EQ.NUMJ   .AND. IBCTYP(2,4).EQ.-3
     &                             .AND. MTBTYP     .EQ. 3) THEN
                    AJP=2.0D0*AXZ*YY(4,J)
     &                  *GGY(I,JP,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                    AUJ(I,J,K)=0.0D0
                  ELSE
                    AJP=0.0D0
                    AUJ(I,J,K)=0.0D0
                  ENDIF
                ELSE
                  AJP=0.0D0
                  AUJ(I,J,K)=0.0D0
                ENDIF
              ELSEIF(ICPL.GT.0) THEN
                AJP=0.0D0
                AUJ(I,J,K)=0.0D0
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- ALK:z方向負側の面 --
              IS=INDZ(I,J,K)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=ZZ(6,K)*( ZZ(2,K )*GGV(I,J,K )/GLV(I,J,K )
     &                      +ZZ(2,KM)*GGV(I,J,KM)/GLV(I,J,KM))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FZ(I,J,K)*RHO0(1)+(1.0D0-FZ(I,J,K))*RHO0(2))
                RHO02 = ZZ(6,K)*( ZZ(2,K )*RHOG(I,J,K )
     &                           +ZZ(2,KM)*RHOG(I,J,KM))
                RI=1.0D0/(FZ(I,J,K)*RHO0(1)+(1.0D0-FZ(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                AKM=AXY*ZZ(5,K)*GGZ(I,J,K)*GV*RI
                ALK(I,J,K)=-AKM
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
                RHO02 = RHOG(I,J,K)
                RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                IF (INDB(LGBF,IS).NE.4) THEN
                  AKM=0.0D0
                  ALK(I,J,K)=0.0D0
                ELSE
                  AKM=2.0D0*AXY*ZZ(4,K)
     &                *GGZ(I,J,K)*GGV(I,J,K)/GLV(I,J,K)*RI
                  ALK(I,J,K)=0.0D0
                ENDIF
              ELSEIF(ICPL.GT.0) THEN
                AKM=0.0D0
                ALK(I,J,K)=0.0D0
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AUK:z方向正側の面 --
              IS=INDZ(I,J,KP)
CD            * 通常の面
              IF     (IS.EQ.0) THEN
                GV=ZZ(6,KP)*( ZZ(2,KP)*GGV(I,J,KP)/GLV(I,J,KP)
     &                       +ZZ(2,K )*GGV(I,J,K )/GLV(I,J,K ))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FZ(I,J,KP)*RHO0(1)+(1.0D0-FZ(I,J,KP))*RHO0(2))
                RHO02 = ZZ(6,KP)*( ZZ(2,KP)*RHOG(I,J,KP)
     &                            +ZZ(2,K )*RHOG(I,J,K ))
                RI=1.0D0/(FZ(I,J,KP)*RHO0(1)+(1.0D0-FZ(I,J,KP))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                AKP=AXY*ZZ(5,KP)*GGZ(I,J,KP)*GV*RI
                AUK(I,J,K)=-AKP
CD            * 境界の面
              ELSEIF (IS.GE.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
                RHO02 = RHOG(I,J,K)
                RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                IF (INDB(LGBF,IS).NE.4) THEN
                  AKP=0.0D0
                  AUK(I,J,K)=0.0D0
                ELSE
                  AKP=2.0D0*AXY*ZZ(4,K)
     &                *GGZ(I,J,KP)*GGV(I,J,K)/GLV(I,J,K)*RI
                  AUK(I,J,K)=0.0D0
                ENDIF
              ELSEIF(ICPL.GT.0) THEN
                AKP=0.0D0
                AUK(I,J,K)=0.0D0
              ELSE
                CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
              ENDIF

CD            -- AD:対角項 --
              AD(I,J,K)=AKM+AJM+AIM+AIP+AJP+AKP

CD            -- BB:右辺 --
              VD= AYZ*(GGX(IP,J,K)*UU(IP,J,K)-GGX(I,J,K)*UU(I,J,K))
     &           +AXZ*(GGY(I,JP,K)*VV(I,JP,K)-GGY(I,J,K)*VV(I,J,K))
     &           +AXY*(GGZ(I,J,KP)*WW(I,J,KP)-GGZ(I,J,K)*WW(I,J,K))
              IF(ICPL.GT.0 .OR. ISTM.EQ.1) THEN
                VQ=-(GGV(I,J,K)-GGV0(I,J,K))*DTI*AVL
              ELSE
                VQ=0.0D0
              ENDIF
CAKIY         IF (IWS.EQ.I) VQ=2.0D0*ZZ(2,K)*WVU(K)
              BB(I,J,K)=VD-VQ
C@@@@
      DVQQQ=DVQQQ+ABS(VQ)
C@@@@

CD          -- 表面セル:z負方向に流体 --
C2F         ELSEIF (N.EQ.5) THEN
C2F           PSI=ZZ(3,K  )/(0.5D0*ZZ(2,K-1)+FF(I,J,K)*ZZ(2,K))
C2F           AD (I,J,K)=1.0D0
C2F           ALI(I,J,K)=0.0D0
C2F           ALJ(I,J,K)=0.0D0
C2F           ALK(I,J,K)=PSI-1.0D0
C2F           AUI(I,J,K)=0.0D0
C2F           AUJ(I,J,K)=0.0D0
C2F           AUK(I,J,K)=0.0D0
C2F           BB (I,J,K)=
C2F  &          -TR*((1.0D0-PSI)*PP(I,J,K-1)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:z正方向に流体 --
C2F         ELSEIF (N.EQ.6) THEN
C2F           PSI=ZZ(3,K+1)/(0.5D0*ZZ(2,K+1)+FF(I,J,K)*ZZ(2,K))
C2F           AD (I,J,K)=1.0D0
C2F           ALI(I,J,K)=0.0D0
C2F           ALJ(I,J,K)=0.0D0
C2F           ALK(I,J,K)=0.0D0
C2F           AUI(I,J,K)=0.0D0
C2F           AUJ(I,J,K)=0.0D0
C2F           AUK(I,J,K)=PSI-1.0D0
C2F           BB (I,J,K)=
C2F  &          -TR*((1.0D0-PSI)*PP(I,J,K+1)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:y負方向に流体 --
C2F         ELSEIF (N.EQ.3) THEN
C2F           PSI=YY(3,J  )/(0.5D0*YY(2,J-1)+FF(I,J,K)*YY(2,J))
C2F           AD (I,J,K)=1.0D0
C2F           ALI(I,J,K)=0.0D0
C2F           ALJ(I,J,K)=PSI-1.0D0
C2F           ALK(I,J,K)=0.0D0
C2F           AUI(I,J,K)=0.0D0
C2F           AUJ(I,J,K)=0.0D0
C2F           AUK(I,J,K)=0.0D0
C2F           BB (I,J,K)=
C2F  &          -TR*((1.0D0-PSI)*PP(I,J-1,K)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:y正方向に流体 --
C2F         ELSEIF (N.EQ.4) THEN
C2F           PSI=YY(3,J+1)/(0.5D0*YY(2,J+1)+FF(I,J,K)*YY(2,J))
C2F           AD (I,J,K)=1.0D0
C2F           ALI(I,J,K)=0.0D0
C2F           ALJ(I,J,K)=0.0D0
C2F           ALK(I,J,K)=0.0D0
C2F           AUI(I,J,K)=0.0D0
C2F           AUJ(I,J,K)=PSI-1.0D0
C2F           AUK(I,J,K)=0.0D0
C2F           BB (I,J,K)=
C2F  &          -TR*((1.0D0-PSI)*PP(I,J+1,K)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:x負方向に流体 --
C2F         ELSEIF (N.EQ.1) THEN
C2F           PSI=XX(3,I  )/(0.5D0*XX(2,I-1)+FF(I,J,K)*XX(2,I))
C2F           AD (I,J,K)=1.0D0
C2F           ALI(I,J,K)=PSI-1.0D0
C2F           ALJ(I,J,K)=0.0D0
C2F           ALK(I,J,K)=0.0D0
C2F           AUI(I,J,K)=0.0D0
C2F           AUJ(I,J,K)=0.0D0
C2F           AUK(I,J,K)=0.0D0
C2F           BB (I,J,K)=
C2F  &          -TR*((1.0D0-PSI)*PP(I-1,J,K)+PSI*PPS-PP(I,J,K))

CD          -- 表面セル:x正方向に流体 --
C2F         ELSEIF (N.EQ.2) THEN
C2F           PSI=XX(3,I+1)/(0.5D0*XX(2,I+1)+FF(I,J,K)*XX(2,I))
C2F           AD (I,J,K)=1.0D0
C2F           ALI(I,J,K)=0.0D0
C2F           ALJ(I,J,K)=0.0D0
C2F           ALK(I,J,K)=0.0D0
C2F           AUI(I,J,K)=PSI-1.0D0
C2F           AUJ(I,J,K)=0.0D0
C2F           AUK(I,J,K)=0.0D0
C2F           BB (I,J,K)=
C2F  &          -TR*((1.0D0-PSI)*PP(I+1,J,K)+PSI*PPS-PP(I,J,K))

CD          -- プログラムエラー --
            ELSE
              CALL VF_A2ERR('VF_VPCOEF','P.G ERROR.')
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE
C@@@@
C@      WRITE(*,*) 'DVQQQ CC',DVQQQ
C@@@@

C@      CALL VF_P3SRD1(AD ,DBUF,0)
C@      CALL VF_P3SRD1(ALI,DBUF,0)
C@      CALL VF_P3SRD1(ALJ,DBUF,0)
C@      CALL VF_P3SRD1(ALK,DBUF,0)
C@      CALL VF_P3SRD1(AUI,DBUF,0)
C@      CALL VF_P3SRD1(AUJ,DBUF,0)
C@      CALL VF_P3SRD1(AUK,DBUF,0)
C@      CALL VF_P3SRD1(BB ,DBUF,0)

C     -- 実行文の終了 --
CDBG      ISW=0
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<AD>>'
CDBG      CALL VF_OL3DR(AD,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<ALI>>'
CDBG      CALL VF_OL3DR(ALI,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<ALJ>>'
CDBG      CALL VF_OL3DR(ALJ,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<ALK>>'
CDBG      CALL VF_OL3DR(ALK,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<AUI>>'
CDBG      CALL VF_OL3DR(AUI,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<AUJ>>'
CDBG      CALL VF_OL3DR(AUJ,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<AUK>>'
CDBG      CALL VF_OL3DR(AUK,ISW)
CDBG      WRITE(ILPFIL,'(A)') ' VF_VPCOEF: <<BB>>'
CDBG      CALL VF_OL3DR(BB,ISW)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
