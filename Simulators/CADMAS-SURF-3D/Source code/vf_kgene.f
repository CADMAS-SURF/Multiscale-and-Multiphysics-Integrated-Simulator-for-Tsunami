      SUBROUTINE VF_KGENE(XX,YY,ZZ,UU,VV,WW,GGV,BCU,BCV,BCW,
     &                    ANUT,AK,AE,QK,QE,NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_KGENE:乱流量の生成消滅項の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
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
CD    BCU(NUMB)        : IN  : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : IN  : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : IN  : R*8 : z方向流速の境界値
CD    ANUT(@FOR-3D@)   : IN  : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ散逸
CD    QK(@FOR-3D@)     : I/O : R*8 : kの生成消滅
CD    QE(@FOR-3D@)     : I/O : R*8 : εの生成消滅
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),GGV (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB)
      DIMENSION ANUT(NUMI,NUMJ,NUMK)
      DIMENSION AK  (NUMI,NUMJ,NUMK),AE  (NUMI,NUMJ,NUMK)
      DIMENSION QK  (NUMI,NUMJ,NUMK),QE  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 生成消滅項の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.0) THEN
              IM=I-1
              IP=I+1
              JM=J-1
              JP=J+1
              KM=K-1
              KP=K+1

C             * 定義点の流速値
              UXM=UU(I ,J,K)
              UXP=UU(IP,J,K)
              VYM=VV(I,J ,K)
              VYP=VV(I,JP,K)
              WZM=WW(I,J,K )
              WZP=WW(I,J,KP)

C             * 未定義点の流速値
              IND=INDX(I ,J,K)
              IF (IND.EQ.0) THEN
                VXM=0.5D0*XX(6,I )
     &                 *( XX(2,IM)*(VV(I ,J,K)+VV(I ,JP,K))
     &                   +XX(2,I )*(VV(IM,J,K)+VV(IM,JP,K)))
                WXM=0.5D0*XX(6,I )
     &                 *( XX(2,IM)*(WW(I ,J,K)+WW(I ,J,KP))
     &                   +XX(2,I )*(WW(IM,J,K)+WW(IM,J,KP)))
              ELSE
                VXM=BCV(IND)
                WXM=BCW(IND)
              ENDIF
              IND=INDX(IP,J,K)
              IF (IND.EQ.0) THEN
                VXP=0.5D0*XX(6,IP)
     &                 *( XX(2,I )*(VV(IP,J,K)+VV(IP,JP,K))
     &                   +XX(2,IP)*(VV(I ,J,K)+VV(I ,JP,K)))
                WXP=0.5D0*XX(6,IP)
     &                 *( XX(2,I )*(WW(IP,J,K)+WW(IP,J,KP))
     &                   +XX(2,IP)*(WW(I ,J,K)+WW(I ,J,KP)))
              ELSE
                VXP=BCV(IND)
                WXP=BCW(IND)
              ENDIF
              IND=INDY(I,J ,K)
              IF (IND.EQ.0) THEN
                UYM=0.5D0*YY(6,J )
     &                 *( YY(2,JM)*(UU(I,J ,K)+UU(IP,J ,K))
     &                   +YY(2,J )*(UU(I,JM,K)+UU(IP,JM,K)))
                WYM=0.5D0*YY(6,J )
     &                 *( YY(2,JM)*(WW(I,J ,K)+WW(I,J ,KP))
     &                   +YY(2,J )*(WW(I,JM,K)+WW(I,JM,KP)))
              ELSE
                UYM=BCU(IND)
                WYM=BCW(IND)
              ENDIF
              IND=INDY(I,JP,K)
              IF (IND.EQ.0) THEN
                UYP=0.5D0*YY(6,JP)
     &                 *( YY(2,J )*(UU(I,JP,K)+UU(IP,JP,K))
     &                   +YY(2,JP)*(UU(I,J ,K)+UU(IP,J ,K)))
                WYP=0.5D0*YY(6,JP)
     &                 *( YY(2,J )*(WW(I,JP,K)+WW(I,JP,KP))
     &                   +YY(2,JP)*(WW(I,J ,K)+WW(I,J ,KP)))
              ELSE
                UYP=BCU(IND)
                WYP=BCW(IND)
              ENDIF
              IND=INDZ(I,J,K )
              IF (IND.EQ.0) THEN
                UZM=0.5D0*ZZ(6,K )
     &                 *( ZZ(2,KM)*(UU(I,J,K )+UU(IP,J,K ))
     &                   +ZZ(2,K )*(UU(I,J,KM)+UU(IP,J,KM)))
                VZM=0.5D0*ZZ(6,K )
     &                 *( ZZ(2,KM)*(VV(I,J,K )+VV(I,JP,K ))
     &                   +ZZ(2,K )*(VV(I,J,KM)+VV(I,JP,KM)))
              ELSE
                UZM=BCU(IND)
                VZM=BCV(IND)
              ENDIF
              IND=INDZ(I,J,KP)
              IF (IND.EQ.0) THEN
                UZP=0.5D0*ZZ(6,KP)
     &                 *( ZZ(2,K )*(UU(I,J,KP)+UU(IP,J,KP))
     &                   +ZZ(2,KP)*(UU(I,J,K )+UU(IP,J,K )))
                VZP=0.5D0*ZZ(6,KP)
     &                 *( ZZ(2,K )*(VV(I,J,KP)+VV(I,JP,KP))
     &                   +ZZ(2,KP)*(VV(I,J,K )+VV(I,JP,K )))
              ELSE
                UZP=BCU(IND)
                VZP=BCV(IND)
              ENDIF

C             * 流速の微分値
              DLR=XX(4,I)
              DUDX=DLR*(UXP-UXM)
              DVDX=DLR*(VXP-VXM)
              DWDX=DLR*(WXP-WXM)
              DLR=YY(4,J)
              DUDY=DLR*(UYP-UYM)
              DVDY=DLR*(VYP-VYM)
              DWDY=DLR*(WYP-WYM)
              DLR=ZZ(4,K)
              DUDZ=DLR*(UZP-UZM)
              DVDZ=DLR*(VZP-VZM)
              DWDZ=DLR*(WZP-WZM)

C             * GSの計算
              GS=ANUT(I,J,K)*(2.0D0*( DUDX*DUDX
     &                               +DVDY*DVDY
     &                               +DWDZ*DWDZ)
     &                        +(DUDY+DVDX)**2
     &                        +(DVDZ+DWDY)**2
     &                        +(DWDX+DUDZ)**2   )

CAKIY         * GT,RFの計算
              GT =0.0D0
              GST=GS+GT
              RF =-GT/MAX(GST,ZERO)

CD            * ゼロ割防止等
              AK0=MAX(AK(I,J,K),AKMINK)
              AE0=AE(I,J,K)
              AEK=AE0/AK0

CD            * 生成消滅項の計算
              GG=GGV(I,J,K)
              QK(I,J,K)=QK(I,J,K)+GG*(GST-AE0)
              QE(I,J,K)=QE(I,J,K)+GG*( AKC1*AEK*GST*(1.0D0+AKC3*RF)
     &                                -AKC2*AEK*AE0                )
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
