      SUBROUTINE VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                    NF,INDX,INDY,INDZ,ISW0,ISW,IIN,JIN,KIN,VORT)

CD=== 概要 ===========================================================

CDT   VF_CVORT:セル毎の渦度の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    BCU(NUMB)        : IN  : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : IN  : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : IN  : R*8 : z方向流速の境界値
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    ISW0             : IN  : I*4 : インデックスのフラグ
CD                                   = 0:シフトが必要ない
CD                                   =!0:シフトが必要
CD    ISW              : IN  : I*4 : 成分フラグ(=1:X,=2:Y,=3:Z)
CD    IIN              : IN  : I*4 : セルのI方向インデックス
CD    JIN              : IN  : I*4 : セルのJ方向インデックス
CD    KIN              : IN  : I*4 : セルのK方向インデックス
CD    VORT             : OUT : R*8 : 渦度
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分等 --
      IF (ISW0.EQ.0) THEN
        I0=IIN
        J0=JIN
        K0=KIN
        LON=0
      ELSE
        I0=IIN-(MYGIS-1)
        J0=JIN-(MYGJS-1)
        K0=KIN
        LON=1
        IF (MYIS.LE.I0 .AND. I0.LE.MYIE .AND.
     &      MYJS.LE.J0 .AND. J0.LE.MYJE      ) THEN
          LON=0
        ENDIF
      ENDIF

      VORT=0.0D0
      IF (LON.EQ.0) THEN

C       -- 渦度(x方向成分) --
        IF     (ISW.EQ.1) THEN
          VORT=0.0D0
          IF (NF(I0,J0,K0).EQ.0) THEN
            JM=J0-1
            JP=J0+1
            KM=K0-1
            KP=K0+1
            IND=INDY(I0,J0,K0)
            IF (IND.EQ.0) THEN
              WYM=0.5D0*YY(6,J0)
     &               *( YY(2,JM)*(WW(I0,J0,K0)+WW(I0,J0,KP))
     &                 +YY(2,J0)*(WW(I0,JM,K0)+WW(I0,JM,KP)))
            ELSE
              WYM=BCW(IND)
            ENDIF
            IND=INDY(I0,JP,K0)
            IF (IND.EQ.0) THEN
              WYP=0.5D0*YY(6,JP)
     &               *( YY(2,J0)*(WW(I0,JP,K0)+WW(I0,JP,KP))
     &                 +YY(2,JP)*(WW(I0,J0,K0)+WW(I0,J0,KP)))
            ELSE
              WYP=BCW(IND)
            ENDIF
            IND=INDZ(I0,J0,K0)
            IF (IND.EQ.0) THEN
              VZM=0.5D0*ZZ(6,K0)
     &               *( ZZ(2,KM)*(VV(I0,J0,K0)+VV(I0,JP,K0))
     &                 +ZZ(2,K0)*(VV(I0,J0,KM)+VV(I0,JP,KM)))
            ELSE
              VZM=BCV(IND)
            ENDIF
            IND=INDZ(I0,J0,KP)
            IF (IND.EQ.0) THEN
              VZP=0.5D0*ZZ(6,KP)
     &               *( ZZ(2,K0)*(VV(I0,J0,KP)+VV(I0,JP,KP))
     &                 +ZZ(2,KP)*(VV(I0,J0,K0)+VV(I0,JP,K0)))
            ELSE
              VZP=BCV(IND)
            ENDIF
            VORT=YY(4,J0)*(WYP-WYM)-ZZ(4,K0)*(VZP-VZM)
          ENDIF

C       -- 渦度(y方向成分) --
        ELSEIF (ISW.EQ.2) THEN
          VORT=0.0D0
          IF (NF(I0,J0,K0).EQ.0) THEN
            KM=K0-1
            KP=K0+1
            IM=I0-1
            IP=I0+1
            IND=INDZ(I0,J0,K0)
            IF (IND.EQ.0) THEN
              UZM=0.5D0*ZZ(6,K0)
     &               *( ZZ(2,KM)*(UU(I0,J0,K0)+UU(IP,J0,K0))
     &                 +ZZ(2,K0)*(UU(I0,J0,KM)+UU(IP,J0,KM)))
            ELSE
              UZM=BCU(IND)
            ENDIF
            IND=INDZ(I0,J0,KP)
            IF (IND.EQ.0) THEN
              UZP=0.5D0*ZZ(6,KP)
     &               *( ZZ(2,K0)*(UU(I0,J0,KP)+UU(IP,J0,KP))
     &                 +ZZ(2,KP)*(UU(I0,J0,K0)+UU(IP,J0,K0)))
            ELSE
              UZP=BCU(IND)
            ENDIF
            IND=INDX(I0,J0,K0)
            IF (IND.EQ.0) THEN
              WXM=0.5D0*XX(6,I0)
     &               *( XX(2,IM)*(WW(I0,J0,K0)+WW(I0,J0,KP))
     &                 +XX(2,I0)*(WW(IM,J0,K0)+WW(IM,J0,KP)))
            ELSE
              WXM=BCW(IND)
            ENDIF
            IND=INDX(IP,J0,K0)
            IF (IND.EQ.0) THEN
              WXP=0.5D0*XX(6,IP)
     &               *( XX(2,I0)*(WW(IP,J0,K0)+WW(IP,J0,KP))
     &                 +XX(2,IP)*(WW(I0,J0,K0)+WW(I0,J0,KP)))
            ELSE
              WXP=BCW(IND)
            ENDIF
            VORT=ZZ(4,K0)*(UZP-UZM)-XX(4,I0)*(WXP-WXM)
          ENDIF

C       -- 渦度(z方向成分) --
        ELSEIF (ISW.EQ.3) THEN
          VORT=0.0D0
          IF (NF(I0,J0,K0).EQ.0) THEN
            IM=I0-1
            IP=I0+1
            JM=J0-1
            JP=J0+1
            IND=INDX(I0,J0,K0)
            IF (IND.EQ.0) THEN
              VXM=0.5D0*XX(6,I0)
     &               *( XX(2,IM)*(VV(I0,J0,K0)+VV(I0,JP,K0))
     &                 +XX(2,I0)*(VV(IM,J0,K0)+VV(IM,JP,K0)))
            ELSE
              VXM=BCV(IND)
            ENDIF
            IND=INDX(IP,J0,K0)
            IF (IND.EQ.0) THEN
              VXP=0.5D0*XX(6,IP)
     &               *( XX(2,I0)*(VV(IP,J0,K0)+VV(IP,JP,K0))
     &                 +XX(2,IP)*(VV(I0,J0,K0)+VV(I0,JP,K0)))
            ELSE
              VXP=BCV(IND)
            ENDIF
            IND=INDY(I0,J0,K0)
            IF (IND.EQ.0) THEN
              UYM=0.5D0*YY(6,J0)
     &               *( YY(2,JM)*(UU(I0,J0,K0)+UU(IP,J0,K0))
     &                 +YY(2,J0)*(UU(I0,JM,K0)+UU(IP,JM,K0)))
            ELSE
              UYM=BCU(IND)
            ENDIF
            IND=INDY(I0,JP,K0)
            IF (IND.EQ.0) THEN
              UYP=0.5D0*YY(6,JP)
     &               *( YY(2,J0)*(UU(I0,JP,K0)+UU(IP,JP,K0))
     &                 +YY(2,JP)*(UU(I0,J0,K0)+UU(IP,J0,K0)))
            ELSE
              UYP=BCU(IND)
            ENDIF
            VORT=XX(4,I0)*(VXP-VXM)-YY(4,J0)*(UYP-UYM)
          ENDIF

        ELSE
          CALL VF_A2ERR('VF_CVORT','P.G ERROR.')
        ENDIF

      ENDIF

      IF (ISW0.NE.0) THEN
        W=VORT
        CALL VF_P1SUMD(W,VORT)
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
