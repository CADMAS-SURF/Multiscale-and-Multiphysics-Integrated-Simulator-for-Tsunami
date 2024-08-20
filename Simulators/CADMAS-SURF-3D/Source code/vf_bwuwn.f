      SUBROUTINE VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,
     &                    DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &                    DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,DBUF,
     &                    UB,VB,WB,NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWUWN:境界面の法線方向流速および一部の接線方向流速を設定する
CD      (1)接線方向流速は流速を指定している以下の条件のとき設定
CD      (2)ノンスリップ、流速固定、造波境界

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
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : I/O : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
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
CD    UB(@FOR-3D@)     : IN  : R*8 : x方向流速(前時刻)
CD    VB(@FOR-3D@)     : IN  : R*8 : y方向流速(前時刻)
CD    WB(@FOR-3D@)     : IN  : R*8 : z方向流速(前時刻)
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB)
      DIMENSION DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
      DIMENSION DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)
      DIMENSION DMTBTT2(MTBTT2),DMTBZZ2(MTBZZ2),DMTBHH2(MTBTT2)
      DIMENSION DMTBUN2(MTBZZ2,MTBTT2),DMTBUT2(MTBZZ2,MTBTT2)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION UB  (NUMI,NUMJ,NUMK),VB  (NUMI,NUMJ,NUMK)
      DIMENSION WB  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C     -- 局所変数 --
      REAL*4 R4TN

      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: BCX,BCY

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 境界面の流速を設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        IB =INDB(3,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

CD      -- スリップ --
        IF     (IB.EQ.1) THEN
          IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
            BCU(L)   =0.0D0
            UU(I,J,K)=0.0D0
          ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
            BCV(L)   =0.0D0
            VV(I,J,K)=0.0D0
          ELSE
            BCW(L)   =0.0D0
            WW(I,J,K)=0.0D0
          ENDIF

CD      -- ノンスリップ --
        ELSEIF (IB.EQ.2) THEN
          BCU(L)=0.0D0
          BCV(L)=0.0D0
          BCW(L)=0.0D0
          IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
            UU(I,J,K)=BCU(L)
          ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
            VV(I,J,K)=BCV(L)
          ELSE
            WW(I,J,K)=BCW(L)
          ENDIF

CD      -- 流速固定 --
        ELSEIF (IB.EQ.3) THEN
          BCU(L)=BCU(L)
          BCV(L)=BCV(L)
          BCW(L)=BCW(L)
          IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
            UU(I,J,K)=BCU(L)
          ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
            VV(I,J,K)=BCV(L)
          ELSE
            WW(I,J,K)=BCW(L)
          ENDIF

CD      -- フリー --
        ELSEIF (IB.EQ.4) THEN
          IF     (NS.EQ.1) THEN
            VAL      =UU(I+1,J,K)
            BCU(L)   =VAL
            UU(I,J,K)=VAL
          ELSEIF (NS.EQ.2) THEN
            VAL      =UU(I-1,J,K)
            BCU(L)   =VAL
            UU(I,J,K)=VAL
          ELSEIF (NS.EQ.3) THEN
            VAL      =VV(I,J+1,K)
            BCV(L)   =VAL
            VV(I,J,K)=VAL
          ELSEIF (NS.EQ.4) THEN
            VAL      =VV(I,J-1,K)
            BCV(L)   =VAL
            VV(I,J,K)=VAL
          ELSEIF (NS.EQ.5) THEN
            VAL      =WW(I,J,K+1)
            BCW(L)   =VAL
            WW(I,J,K)=VAL
          ELSEIF (NS.EQ.6) THEN
            VAL      =WW(I,J,K-1)
            BCW(L)   =VAL
            WW(I,J,K)=VAL
          ENDIF

CD      -- 造波境界 --
        ELSEIF (IB.EQ.5) THEN
C         * 水位固定のみの処理
C         * その他は特殊境界として本ルーチンの後方で再設定
          IF     (NS.EQ.1) THEN
            IF (I.EQ.2    .AND. IBCTYP(2,1).EQ.-3
     &                    .AND. MTBTYP     .EQ. 3) THEN
              VAL      =UU(I+1,J,K)
              BCU(L)   =VAL
              UU(I,J,K)=VAL
            ENDIF
            IF (I.EQ.2    .AND. IBCTYP(2,1).EQ.-4
     &                    .AND. MTBTYP2    .EQ. 3) THEN
              VAL      =UU(I+1,J,K)
              BCU(L)   =VAL
              UU(I,J,K)=VAL
            ENDIF
          ELSEIF (NS.EQ.2) THEN
            IF (I.EQ.NUMI .AND. IBCTYP(2,2).EQ.-3
     &                    .AND. MTBTYP     .EQ. 3) THEN
              VAL      =UU(I-1,J,K)
              BCU(L)   =VAL
              UU(I,J,K)=VAL
            ENDIF
            IF (I.EQ.NUMI .AND. IBCTYP(2,2).EQ.-4
     &                    .AND. MTBTYP2    .EQ. 3) THEN
              VAL      =UU(I-1,J,K)
              BCU(L)   =VAL
              UU(I,J,K)=VAL
            ENDIF
          ELSEIF (NS.EQ.3) THEN
            IF (J.EQ.2    .AND. IBCTYP(2,3).EQ.-3
     &                    .AND. MTBTYP     .EQ. 3) THEN
              VAL      =VV(I,J+1,K)
              BCV(L)   =VAL
              VV(I,J,K)=VAL
            ENDIF
            IF (J.EQ.2    .AND. IBCTYP(2,3).EQ.-4
     &                    .AND. MTBTYP2    .EQ. 3) THEN
              VAL      =VV(I,J+1,K)
              BCV(L)   =VAL
              VV(I,J,K)=VAL
            ENDIF
          ELSEIF (NS.EQ.4) THEN
            IF (J.EQ.NUMJ .AND. IBCTYP(2,4).EQ.-3
     &                    .AND. MTBTYP     .EQ. 3) THEN
              VAL      =VV(I,J-1,K)
              BCV(L)   =VAL
              VV(I,J,K)=VAL
            ENDIF
            IF (J.EQ.NUMJ .AND. IBCTYP(2,4).EQ.-4
     &                    .AND. MTBTYP2    .EQ. 3) THEN
              VAL      =VV(I,J-1,K)
              BCV(L)   =VAL
              VV(I,J,K)=VAL
            ENDIF
          ELSE
            CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
          ENDIF

CD      -- 対数則 --
        ELSEIF (IB.EQ.6) THEN
          IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
            BCU(L)   =0.0D0
            UU(I,J,K)=0.0D0
          ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
            BCV(L)   =0.0D0
            VV(I,J,K)=0.0D0
          ELSE
            BCW(L)   =0.0D0
            WW(I,J,K)=0.0D0
          ENDIF

CD      -- 放射境界 --
        ELSEIF (IB.EQ.7) THEN
C         * 特殊境界として本ルーチンの後方で設定

CD      -- 完全粗面 --
        ELSEIF (IB.EQ.8) THEN
          IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
            BCU(L)   =0.0D0
            UU(I,J,K)=0.0D0
          ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
            BCV(L)   =0.0D0
            VV(I,J,K)=0.0D0
          ELSE
            BCW(L)   =0.0D0
            WW(I,J,K)=0.0D0
          ENDIF

CD      -- プログラムエラー --
        ELSE
          CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
        ENDIF
 100  CONTINUE

CD    -- 特殊境界の流速を設定する --
      DO 500 JD=1,4

CD      -- 法線方向への造波境界 --
        IF     (IBCTYP(1,JD).EQ.1) THEN
          N =IBCTYP(2,JD)
          D =BCTYP (1,JD)
          H =BCTYP (2,JD)
          T =BCTYP (3,JD)
          DL=BCTYP (4,JD)
          T0=BCTYP (6,JD)
          A =BCTYP (8,JD)
          SA=BCTYP (9,JD)
          SX=BCTYP (10,JD)
          SY=BCTYP (11,JD)
C         * 造波境界のための無次元位相の計算
C           CADMAS-SURFとの結果が変わらないよう、倍精度は別に計算
          TN=T0-TNOW/T
          TN=TN-DBLE(INT(TN))
          IF (TN.LT.0.0D0) TN=TN+1.0D0
          R4TN=REAL(T0-TNOW/T)
          R4TN=R4TN-REAL(INT(R4TN))
          IF (R4TN.LT.0.0E0) R4TN=R4TN+1.0E0
C         * 増幅率の計算
          AW=1.0D0
          IF (A.GE.ZERO) THEN
            A=TNOW/T/A
            IF (A.LT.1.0D0) AW=0.5D0*SIN(PI*(A-0.5D0))+0.5D0
          ENDIF
C         * 造波のための関数を初期化する
          CALL VF_CWMAK0(D,T,H,WLN,N)
C         * 期待する水位を計算する
          WVT=DBLE(R4TN)
          IF     (N.EQ.-3) THEN
            CALL VF_CWMTB1 (WVT,WMT1,WMT2,WVZ,DMTBTT ,DMTBHH )
          ELSEIF (N.EQ.-4) THEN
            CALL VF_CWMTB12(WVT,WMT1,WMT2,WVZ,DMTBTT2,DMTBHH2)
          ELSE
            CALL VF_CWMAK1(WVT,WVZ,N)
          ENDIF
          BCTYP(7,JD)=WVZ*AW
C         * 斜め入射用
          IF (ABS(SA).LE.ZERO) THEN
            ISA=0
            IF     (JD.EQ.1) THEN
              SA= 1.0D0
              SB= 0.0D0
            ELSEIF (JD.EQ.2) THEN
              SA=-1.0D0
              SB= 0.0D0
            ELSEIF (JD.EQ.3) THEN
              SA= 0.0D0
              SB= 1.0D0
            ELSE
              SA= 0.0D0
              SB=-1.0D0
            ENDIF
            SC=0.0D0
          ELSE
            ISA=1
            IF      (JD.EQ.1) THEN
              SA=SA
             ELSEIF (JD.EQ.2) THEN
              SA=SA+180.0D0
             ELSEIF (JD.EQ.3) THEN
              SA=SA+90.0D0
            ELSE
              SA=SA+270.0D0
            ENDIF
            SA=SA*PI/180.0D0
            SB=SIN(SA)
            SA=COS(SA)
            SC=-(SA*SX+SB*SY)
          ENDIF
          IF     (N.EQ.-3 .AND. MTBTYP .EQ.3) THEN
          ELSEIF (N.EQ.-4 .AND. MTBTYP2.EQ.3) THEN
          ELSE
C           * x方向境界面の流速および水位の計算
            IF (JD.EQ.1 .OR. JD.EQ.2) THEN
              IF (JD.EQ.1) THEN
                I =2
                IC=2
              ELSE
                I =NUMI0
                IC=NUMI0-1
              ENDIF
              IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                X0=XX(1,I-IP)
                DO 230 J=IBCTYP(3,JD),IBCTYP(4,JD)
                  IF (MYGJS+1.LE.J .AND. J.LE.MYGJE-1) THEN
                    Y0=(YY(1,J-JP)+YY(1,J+1-JP))*0.5D0
                    IF (ISA.NE.0) THEN
                      RR=(SA*X0+SB*Y0+SC)/SQRT(SA*SA+SB*SB)
                      R4TN=REAL(T0-(TNOW/T-RR/DL))
                      R4TN=R4TN-REAL(INT(R4TN))
                      IF (R4TN.LT.0.0E0) R4TN=R4TN+1.0E0
                      WVT=DBLE(R4TN)
                      IF (N.NE.-3 .AND. N.NE.-4) THEN
                        CALL VF_CWMAK1(WVT,WVZ,N)
                      ELSE
                        CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
                      ENDIF
                    ENDIF
C                   * 現在の水位を計算し、スケーリングを決める
                    IF     (N.EQ.-3 .AND. MTBTYP .NE.1) THEN
                      VWS=1.0D0
                    ELSEIF (N.EQ.-4 .AND. MTBTYP2.NE.1) THEN
                      VWS=1.0D0
                    ELSE
                      VAL=0.0D0
                      DO 200 K=2,NUMK-1
                        NW=NF(IC-IP,J-JP,K)
                        IF     (NW.EQ.-1) THEN
                          VAL=VAL+ZZ(2,K)*1.0D0
                        ELSEIF (NW.EQ. 0) THEN
                          VAL=VAL+ZZ(2,K)*FF(IC-IP,J-JP,K)
                        ELSEIF (NW.NE. 8) THEN
                          VAL=VAL+ZZ(2,K)*FF(IC-IP,J-JP,K)
                          GOTO 210
                        ENDIF
 200                  CONTINUE
 210                  CONTINUE
                      VAL=VAL-(WVLVL-ZZ(1,2))
                      VWS=(WVZ+D)/(VAL+D)
                    ENDIF
C                   * 境界値を計算する
                    KMT=1
                    DO 220 K=2,NUMK-1
                      L=INDX(I-IP,J-JP,K)
                      IF (L.GE.1) THEN
                        ZC=(ZZ(1,K)+ZZ(1,K+1))*0.5D0-WVLVL
                        ZC=VWS*(ZC+D)-D
                        IF     (N.EQ.-3) THEN
                          CALL VF_CWMTB2 (KMT,WMT1,WMT2,ZC,UN,UT,
     &                                    DMTBZZ ,DMTBUN ,DMTBUT )
                        ELSEIF (N.EQ.-4) THEN
                          CALL VF_CWMTB22(KMT,WMT1,WMT2,ZC,UN,UT,
     &                                    DMTBZZ2,DMTBUN2,DMTBUT2)
                        ELSE
                          CALL VF_CWMAK2(WVT,ZC,UN,UT,N)
                        ENDIF
                        BCU(L)=UN*VWS*AW*SA
                        BCV(L)=UN*VWS*AW*SB
                        BCW(L)=UT*VWS*AW
                        UU(I-IP,J-JP,K)=BCU(L)
                      ENDIF
 220                CONTINUE
                  ENDIF
 230            CONTINUE
              ENDIF
C           * y方向境界面の流速および水位の計算
            ELSE
              IF (JD.EQ.3) THEN
                J =2
                JC=2
              ELSE
                J =NUMJ0
                JC=NUMJ0-1
              ENDIF
              IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                Y0=YY(1,J-JP)
                DO 280 I=IBCTYP(3,JD),IBCTYP(4,JD)
                  IF (MYGIS+1.LE.I .AND. I.LE.MYGIE-1) THEN
                    X0=(XX(1,I-IP)+XX(1,I+1-IP))*0.5D0
                    IF (ISA.NE.0) THEN
                      RR=(SA*X0+SB*Y0+SC)/SQRT(SA*SA+SB*SB)
                      R4TN=REAL(T0-(TNOW/T-RR/DL))
                      R4TN=R4TN-REAL(INT(R4TN))
                      IF (R4TN.LT.0.0E0) R4TN=R4TN+1.0E0
                      WVT=DBLE(R4TN)
                      IF (N.NE.-3 .AND. N.NE.-4) THEN
                        CALL VF_CWMAK1(WVT,WVZ,N)
                      ELSE
                        CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
                      ENDIF
                    ENDIF
C                   * 現在の水位を計算し、スケーリングを決める
                    IF     (N.EQ.-3 .AND. MTBTYP .NE.1) THEN
                      VWS=1.0D0
                    ELSEIF (N.EQ.-4 .AND. MTBTYP2.NE.1) THEN
                      VWS=1.0D0
                    ELSE
                      VAL=0.0D0
                      DO 250 K=2,NUMK-1
                        NW=NF(I-IP,JC-JP,K)
                        IF     (NW.EQ.-1) THEN
                          VAL=VAL+ZZ(2,K)*1.0D0
                        ELSEIF (NW.EQ. 0) THEN
                          VAL=VAL+ZZ(2,K)*FF(I-IP,JC-JP,K)
                        ELSEIF (NW.NE. 8) THEN
                          VAL=VAL+ZZ(2,K)*FF(I-IP,JC-JP,K)
                          GOTO 260
                        ENDIF
 250                  CONTINUE
 260                  CONTINUE
                      VAL=VAL-(WVLVL-ZZ(1,2))
                      VWS=(WVZ+D)/(VAL+D)
                    ENDIF
C                   * 境界値を計算する
                    KMT=1
                    DO 270 K=2,NUMK-1
                      L=INDY(I-IP,J-JP,K)
                      IF (L.GE.1) THEN
                        ZC=(ZZ(1,K)+ZZ(1,K+1))*0.5D0-WVLVL
                        ZC=VWS*(ZC+D)-D
                        IF     (N.EQ.-3) THEN
                          CALL VF_CWMTB2 (KMT,WMT1,WMT2,ZC,UN,UT,
     &                                    DMTBZZ ,DMTBUN ,DMTBUT )
                        ELSEIF (N.EQ.-4) THEN
                          CALL VF_CWMTB22(KMT,WMT1,WMT2,ZC,UN,UT,
     &                                    DMTBZZ2,DMTBUN2,DMTBUT2)
                        ELSE
                          CALL VF_CWMAK2(WVT,ZC,UN,UT,N)
                        ENDIF
                        BCU(L)=UN*VWS*AW*SA
                        BCV(L)=UN*VWS*AW*SB
                        BCW(L)=UT*VWS*AW
                        VV(I-IP,J-JP,K)=BCV(L)
                      ENDIF
 270                CONTINUE
                  ENDIF
 280            CONTINUE
              ENDIF
            ENDIF
          ENDIF

CD      -- 法線方向への開境界 --
        ELSEIF (IBCTYP(1,JD).EQ.2) THEN
          C=BCTYP(6,JD)
C         * x方向境界面の流速の計算
          IF (JD.EQ.1 .OR. JD.EQ.2) THEN
            IF (JD.EQ.1) THEN
              I =2
              IC=2
              I2=3
            ELSE
              I =NUMI0
              IC=NUMI0-1
              I2=IC
            ENDIF
            IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
              IF (NNOW.EQ.0) THEN
                ALN=1.0D0
                ALT=1.0D0
              ELSE
                ALN=MIN(C*DTNOW/XX(2,IC-IP),1.0D0)
                ALT=MIN(ALN*2.0D0          ,1.0D0)
              ENDIF
C             * 流体セルに接するセルに設定
              DO 310 K=2,NUMK-1
                DO 300 J=2,NUMJ0-1
                  IF (MYGJS+1.LE.J .AND. J.LE.MYGJE-1) THEN
                    L=INDX(I-IP,J-JP,K)
                    IF (L.GE.1) THEN
                      IF (NF(IC-IP,J-JP,K).EQ.0) THEN
                        VC=(VB(IC-IP,J-JP,K)+VB(IC-IP,J+1-JP,K))*0.5D0
                        WC=(WB(IC-IP,J-JP,K)+WB(IC-IP,J-JP,K+1))*0.5D0
                        BCU(L)=(1.0D0-ALN)*BCU(L)+ALN*UB(I2-IP,J-JP,K)
                        BCV(L)=(1.0D0-ALT)*BCV(L)+ALT*VC
                        BCW(L)=(1.0D0-ALT)*BCW(L)+ALT*WC
                        UU(I-IP,J-JP,K)=BCU(L)
                      ELSE
                        BCU(L)=0.0D0
                        BCV(L)=0.0D0
                        BCW(L)=0.0D0
                        UU(I-IP,J-JP,K)=0.0D0
                      ENDIF
                    ENDIF
                  ENDIF
 300            CONTINUE
 310          CONTINUE
C             * 表面セルに接するセルに設定
              DO 330 K=2,NUMK-1
                DO 320 J=2,NUMJ0-1
                  IF (MYGJS+1.LE.J .AND. J.LE.MYGJE-1) THEN
                    L=INDX(I-IP,J-JP,K)
                    IF (L.GE.1) THEN
                      NW=NF(IC-IP,J-JP,K)
                      IF     (NW.EQ.5) THEN
                        L2=INDX(I-IP,J-JP,K-1)
                        BCU(L)=BCU(L2)
                        BCV(L)=BCV(L2)
                        BCW(L)=BCW(L2)
                        UU(I-IP,J-JP,K)=BCU(L)
                      ELSEIF (NW.EQ.6) THEN
                        L2=INDX(I-IP,J-JP,K+1)
                        BCU(L)=BCU(L2)
                        BCV(L)=BCV(L2)
                        BCW(L)=BCW(L2)
                        UU(I-IP,J-JP,K)=0.0D0
                      ENDIF
                    ENDIF
                  ENDIF
 320            CONTINUE
 330          CONTINUE
            ENDIF
C         * y方向境界面の流速の計算
          ELSE
            IF (JD.EQ.3) THEN
              J =2
              JC=2
              J2=3
            ELSE
              J =NUMJ0
              JC=NUMJ0-1
              J2=JC
            ENDIF
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              IF (NNOW.EQ.0) THEN
                ALN=1.0D0
                ALT=1.0D0
              ELSE
                ALN=MIN(C*DTNOW/YY(2,JC-JP),1.0D0)
                ALT=MIN(ALN*2.0D0          ,1.0D0)
              ENDIF
C             * 流体セルに接するセルに設定
              DO 410 K=2,NUMK-1
                DO 400 I=2,NUMI0-1
                  IF (MYGIS+1.LE.I .AND. I.LE.MYGIE-1) THEN
                    L=INDY(I-IP,J-JP,K)
                    IF (L.GE.1) THEN
                      IF (NF(I-IP,JC-JP,K).EQ.0) THEN
                        UC=(UB(I-IP,JC-JP,K)+UB(I+1-IP,JC-JP,K))*0.5D0
                        WC=(WB(I-IP,JC-JP,K)+WB(I-IP,JC-JP,K+1))*0.5D0
                        BCU(L)=(1.0D0-ALT)*BCU(L)+ALT*UC
                        BCV(L)=(1.0D0-ALN)*BCV(L)+ALN*VB(I-IP,J2-JP,K)
                        BCW(L)=(1.0D0-ALT)*BCW(L)+ALT*WC
                        VV(I-IP,J-JP,K)=BCV(L)
                      ELSE
                        BCU(L)=0.0D0
                        BCV(L)=0.0D0
                        BCW(L)=0.0D0
                        VV(I-IP,J-JP,K)=0.0D0
                      ENDIF
                    ENDIF
                  ENDIF
 400            CONTINUE
 410          CONTINUE
C             * 表面セルに接するセルに設定
              DO 430 K=2,NUMK-1
                DO 420 I=2,NUMI0-1
                  IF (MYGIS+1.LE.I .AND. I.LE.MYGIE-1) THEN
                    L=INDY(I-IP,J-JP,K)
                    IF (L.GE.1) THEN
                      NW=NF(I-IP,JC-JP,K)
                      IF     (NW.EQ.5) THEN
                        L2=INDY(I-IP,J-JP,K-1)
                        BCU(L)=BCU(L2)
                        BCV(L)=BCV(L2)
                        BCW(L)=BCW(L2)
                        VV(I-IP,J-JP,K)=BCV(L)
                      ELSEIF (NW.EQ.6) THEN
                        L2=INDY(I-IP,J-JP,K+1)
                        BCU(L)=BCU(L2)
                        BCV(L)=BCV(L2)
                        BCW(L)=BCW(L2)
                        VV(I-IP,J-JP,K)=BCV(L)
                      ENDIF
                    ENDIF
                  ENDIF
 420            CONTINUE
 430          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 500  CONTINUE

C     --- STOC(U,V,W) ---
      IF (NB_SC.GT.0) THEN
        I1=MIST(1)
        I2=MIST(NIST+1)
        DO 630 KST=1,NKST
          DO 620 JST=1,NJST
            U1=UWST(JST,KST,1)
            V1=UWST(JST,KST,2)
            W1=UWST(JST,KST,3)
            U2=UEST(JST,KST,1)
            V2=UEST(JST,KST,2)
            W2=UEST(JST,KST,3)
            DO 610 K=MKST(KST),MKST(KST+1)-1
              DO 600 J=MJST(JST),MJST(JST+1)-1
                L=INDX(I1,J,K)
                IF (L.GE.1 .AND. IWST.EQ.1) THEN
                  BCU(L)=U1
                  BCV(L)=V1
                  BCW(L)=W1
                  UU(I1,J,K)=U1
                ENDIF
                L=INDX(I2,J,K)
                IF (L.GE.1 .AND. IEST.EQ.1) THEN
                  BCU(L)=U2
                  BCV(L)=V2
                  BCW(L)=W2
                  UU(I2,J,K)=U2
                ENDIF
 600          CONTINUE
 610        CONTINUE
 620      CONTINUE
 630    CONTINUE
        J1=MJST(1)
        J2=MJST(NJST+1)
        DO 730 KST=1,NKST
          DO 720 IST=1,NIST
            U1=VSST(IST,KST,1)
            V1=VSST(IST,KST,2)
            W1=VSST(IST,KST,3)
            U2=VNST(IST,KST,1)
            V2=VNST(IST,KST,2)
            W2=VNST(IST,KST,3)
            DO 710 K=MKST(KST),MKST(KST+1)-1
              DO 700 I=MIST(IST),MIST(IST+1)-1
                L=INDY(I,J1,K)
                IF (L.GE.1 .AND. JSST.EQ.1) THEN
                  BCU(L)=U1
                  BCV(L)=V1
                  BCW(L)=W1
                  VV(I,J1,K)=V1
                ENDIF
                L=INDY(I,J2,K)
                IF (L.GE.1 .AND. JNST.EQ.1) THEN
                  BCU(L)=U2
                  BCV(L)=V2
                  BCW(L)=W2
                  VV(I,J2,K)=V2
                ENDIF
 700          CONTINUE
 710        CONTINUE
 720      CONTINUE
 730    CONTINUE
      ENDIF

      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)


CD    -- STOCと連成かつ領域分割時、接続境界の接線方向流速を通信する --
      IF (NB_SC.GT.0) THEN

      ALLOCATE(BCX(2,NUMK,4),STAT=IERR)
      ALLOCATE(BCY(2,NUMK,4),STAT=IERR)

      DO 110 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        IB =INDB(3,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
C
        IF (IB.EQ.3) THEN
          IF     (NS.EQ.1.AND.I.EQ.2   .AND.J.EQ.1   +MYMJS) THEN
             BCX(1,K,1) = BCV(L)
             BCX(2,K,1) = BCW(L)
          ELSEIF (NS.EQ.1.AND.I.EQ.2   .AND.J.EQ.NUMJ-MYMJE) THEN
             BCX(1,K,3) = BCV(L)
             BCX(2,K,3) = BCW(L)
          ELSEIF (NS.EQ.2.AND.I.EQ.NUMI.AND.J.EQ.1   +MYMJS) THEN
             BCX(1,K,2) = BCV(L)
             BCX(2,K,2) = BCW(L)
          ELSEIF (NS.EQ.2.AND.I.EQ.NUMI.AND.J.EQ.NUMJ-MYMJE) THEN
             BCX(1,K,4) = BCV(L)
             BCX(2,K,4) = BCW(L)
          ELSEIF (NS.EQ.3.AND.J.EQ.2   .AND.I.EQ.1   +MYMIS) THEN
             BCY(1,K,1) = BCU(L)
             BCY(2,K,1) = BCW(L)
          ELSEIF (NS.EQ.3.AND.J.EQ.2   .AND.I.EQ.NUMI-MYMIE) THEN
             BCY(1,K,2) = BCU(L)
             BCY(2,K,2) = BCW(L)
          ELSEIF (NS.EQ.4.AND.J.EQ.NUMJ.AND.I.EQ.1   +MYMIS) THEN
             BCY(1,K,3) = BCU(L)
             BCY(2,K,3) = BCW(L)
          ELSEIF (NS.EQ.4.AND.J.EQ.NUMJ.AND.I.EQ.NUMI-MYMIE) THEN
             BCY(1,K,4) = BCU(L)
             BCY(2,K,4) = BCW(L)
          ENDIF
        ENDIF
 110  CONTINUE

      CALL VF_STOC_1D(BCX,DBUF,1)
      CALL VF_STOC_1D(BCY,DBUF,2)

      DO 120 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        IB =INDB(3,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
C
        IF (IB.EQ.3) THEN
          IF     (NS.EQ.1.AND.I.EQ.2   .AND.J.EQ.       MYMJS) THEN
             BCV(L) = BCX(1,K,1)
             BCW(L) = BCX(2,K,1)
          ELSEIF (NS.EQ.1.AND.I.EQ.2   .AND.J.EQ.NUMJ+1-MYMJE) THEN
             BCV(L) = BCX(1,K,3)
             BCW(L) = BCX(2,K,3)
          ELSEIF (NS.EQ.2.AND.I.EQ.NUMI.AND.J.EQ.       MYMJS) THEN
             BCV(L) = BCX(1,K,2)
             BCW(L) = BCX(2,K,2)
          ELSEIF (NS.EQ.2.AND.I.EQ.NUMI.AND.J.EQ.NUMJ+1-MYMJE) THEN
             BCV(L) = BCX(1,K,4)
             BCW(L) = BCX(2,K,4)
          ELSEIF (NS.EQ.3.AND.J.EQ.2   .AND.I.EQ.       MYMIS) THEN
             BCU(L) = BCY(1,K,1)
             BCW(L) = BCY(2,K,1)
          ELSEIF (NS.EQ.3.AND.J.EQ.2   .AND.I.EQ.NUMI+1-MYMIE) THEN
             BCU(L) = BCY(1,K,2)
             BCW(L) = BCY(2,K,2)
          ELSEIF (NS.EQ.4.AND.J.EQ.NUMJ.AND.I.EQ.       MYMIS) THEN
             BCU(L) = BCY(1,K,3)
             BCW(L) = BCY(2,K,3)
          ELSEIF (NS.EQ.4.AND.J.EQ.NUMJ.AND.I.EQ.NUMI+1-MYMIE) THEN
             BCU(L) = BCY(1,K,4)
             BCW(L) = BCY(2,K,4)
          ENDIF
        ENDIF
 120  CONTINUE

      DEALLOCATE(BCY)
      DEALLOCATE(BCX)
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
