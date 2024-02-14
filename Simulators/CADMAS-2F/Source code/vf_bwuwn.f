      SUBROUTINE VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,BCF,
     &                    DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,DBUF,
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
      INCLUDE 'SF_STRUCT.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : I/O : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCU(NUMB,3)      : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)      : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)      : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    DMTBTT(MTBTT)       : IN : R*8 : マトリクスデータの無次元位相
CD    DMTBZZ(MTBZZ)       : IN : R*8 : マトリクスデータのz座標
CD    DMTBHH(MTBTT)       : IN : R*8 : マトリクスデータの水位
CD    DMTBUN(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの鉛直方向流速
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
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCF(NUMB)
      DIMENSION DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
      DIMENSION DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION UB  (NUMI,NUMJ,NUMK),VB  (NUMI,NUMJ,NUMK)
      DIMENSION WB  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C     -- 局所変数 --
      REAL*4 R4TN

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 境界面の流速を設定する(境界面のみのループ) --
CMVO  DO 100 L=1,NUMB
      IF (NUMB0.LE.0) THEN
        WRITE(*,'(A,I10)') ' PROGRAM ERROR(VF_BWUWN) NUMB0=',NUMB0
        STOP
      ENDIF

      IF(ICPL.EQ.0 .AND. ISTM.EQ.0) THEN
        NUMB1=NUMB0
      ELSE
        NUMB1=NUMB
      ENDIF

CSTR  DO 100 L=1,NUMB0
      DO 100 L=1,NUMB1
        IJK=INDB(1,L)
        IF (IJK.LE.0) GOTO 100
        NS =INDB(2,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

        DO 99 LGF=2,3
          IF (LGF.EQ.2) THEN
            IB =INDB(3,L)
          ELSE
            IB =INDB(5,L)
          ENDIF

CD      -- スリップ --
          IF     (IB.EQ.1) THEN
            IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
              BCU(L,LGF)=0.0D0
            ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
              BCV(L,LGF)=0.0D0
            ELSE
              BCW(L,LGF)=0.0D0
            ENDIF

CD      -- ノンスリップ --
          ELSEIF (IB.EQ.2) THEN
            BCU(L,LGF)=0.0D0
            BCV(L,LGF)=0.0D0
            BCW(L,LGF)=0.0D0

CD      -- 流速固定 --
          ELSEIF (IB.EQ.3) THEN
            BCU(L,LGF)=BCU(L,LGF)
            BCV(L,LGF)=BCV(L,LGF)
            BCW(L,LGF)=BCW(L,LGF)

CD      -- フリー --
          ELSEIF (IB.EQ.4) THEN
            IF     (NS.EQ.1) THEN
              BCU(L,LGF)=UU(I+1,J,K)
            ELSEIF (NS.EQ.2) THEN
              BCU(L,LGF)=UU(I-1,J,K)
            ELSEIF (NS.EQ.3) THEN
              BCV(L,LGF)=VV(I,J+1,K)
            ELSEIF (NS.EQ.4) THEN
              BCV(L,LGF)=VV(I,J-1,K)
            ELSEIF (NS.EQ.5) THEN
              BCW(L,LGF)=WW(I,J,K+1)
            ELSEIF (NS.EQ.6) THEN
              BCW(L,LGF)=WW(I,J,K-1)
            ENDIF

CD      -- 造波境界 --
          ELSEIF (IB.EQ.5) THEN
C         * 水位固定のみの処理
C         * その他は特殊境界として本ルーチンの後方で再設定
            IF     (NS.EQ.1) THEN
              IF (I.EQ.2    .AND. IBCTYP(2,1).EQ.-3
     &                      .AND. MTBTYP     .EQ. 3) THEN
                BCU(L,LGF)=UU(I+1,J,K)
              ENDIF
            ELSEIF (NS.EQ.2) THEN
              IF (I.EQ.NUMI .AND. IBCTYP(2,2).EQ.-3
     &                      .AND. MTBTYP     .EQ. 3) THEN
                BCU(L,LGF)=UU(I-1,J,K)
              ENDIF
            ELSEIF (NS.EQ.3) THEN
              IF (J.EQ.2    .AND. IBCTYP(2,3).EQ.-3
     &                      .AND. MTBTYP     .EQ. 3) THEN
                BCV(L,LGF)=VV(I,J+1,K)
              ENDIF
            ELSEIF (NS.EQ.4) THEN
              IF (J.EQ.NUMJ .AND. IBCTYP(2,4).EQ.-3
     &                      .AND. MTBTYP     .EQ. 3) THEN
                BCV(L,LGF)=VV(I,J-1,K)
              ENDIF
            ELSE
              CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
            ENDIF

CD      -- 対数則 --
          ELSEIF (IB.EQ.6) THEN
            IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
              BCU(L,LGF)=0.0D0
            ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
              BCV(L,LGF)=0.0D0
            ELSE
              BCW(L,LGF)=0.0D0
            ENDIF

CD      -- 放射境界 --
          ELSEIF (IB.EQ.7) THEN
C         * 特殊境界として本ルーチンの後方で設定

CD      -- 完全粗面 --
          ELSEIF (IB.EQ.8) THEN
            IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
              BCU(L,LGF)=0.0D0
            ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
              BCV(L,LGF)=0.0D0
            ELSE
              BCW(L,LGF)=0.0D0
            ENDIF

CD      -- プログラムエラー --
          ELSE
            CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
          ENDIF
 99     CONTINUE

CD      -- 法線方向流速の設定 --
        IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
          BCU(L,1) =BCF(L)*BCU(L,2)+(1.0D0-BCF(L))*BCU(L,3)
          UU(I,J,K)=BCU(L,1)
        ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
          BCV(L,1) =BCF(L)*BCV(L,2)+(1.0D0-BCF(L))*BCV(L,3)
          VV(I,J,K)=BCV(L,1)
        ELSE
          BCW(L,1) =BCF(L)*BCW(L,2)+(1.0D0-BCF(L))*BCW(L,3)
          WW(I,J,K)=BCW(L,1)
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
          IF (N.NE.-3) THEN
            CALL VF_CWMAK1(WVT,WVZ,N)
          ELSE
            CALL VF_CWMTB1(WVT,WMT1,WMT2,WVZ,DMTBTT,DMTBHH)
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
            IF     (JD.EQ.1) THEN
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
          IF (N.NE.-3 .OR. MTBTYP.NE.3) THEN
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
                      IF (N.NE.-3) THEN
                        CALL VF_CWMAK1(WVT,WVZ,N)
                      ELSE
                        CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
                      ENDIF
                    ENDIF
C                   * 現在の水位を計算し、スケーリングを決める
                    IF (N.NE.-3 .OR. MTBTYP.EQ.1) THEN
                      VAL=0.0D0
                      DO 200 K=2,NUMK-1
                        NW=NF(IC-IP,J-JP,K)
                        IF     (NW.LE.-1) THEN
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
                    ELSE
                      VWS=1.0D0
                    ENDIF
C                   * 境界値を計算する
                    KMT=1
                    DO 220 K=2,NUMK-1
                      L=INDX(I-IP,J-JP,K)
                      IF (L.GE.1) THEN
                        ZC=(ZZ(1,K)+ZZ(1,K+1))*0.5D0-WVLVL
                        ZC=VWS*(ZC+D)-D
                        IF (N.NE.-3) THEN
                          CALL VF_CWMAK2(WVT,ZC,UN,UT,N)
                        ELSE
                          CALL VF_CWMTB2(KMT,WMT1,WMT2,ZC,UN,UT,
     &                                   DMTBZZ,DMTBUN,DMTBUT)
                        ENDIF
                        BCU(L,2)=UN*VWS*AW*SA
                        BCV(L,2)=UN*VWS*AW*SB
                        BCW(L,2)=UT*VWS*AW
                        CF1=BCF(L)
                        IF (CF1.GT.0.0D0) THEN
                          BCU(L,1)=BCU(L,2)
                          BCV(L,1)=BCV(L,2)
                          BCW(L,1)=BCW(L,2)
                        ELSE
                          CF2=1.0D0-CF1
                          BCU(L,1)=CF1*BCU(L,2)+CF2*BCU(L,3)
                          BCV(L,1)=CF1*BCV(L,2)+CF2*BCV(L,3)
                          BCW(L,1)=CF1*BCW(L,2)+CF2*BCW(L,3)
                        ENDIF
                        UU(I-IP,J-JP,K)=BCU(L,1)
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
                      IF (N.NE.-3) THEN
                        CALL VF_CWMAK1(WVT,WVZ,N)
                      ELSE
                        CALL VF_A2ERR('VF_BWUWN','P.G ERROR.')
                      ENDIF
                    ENDIF
C                   * 現在の水位を計算し、スケーリングを決める
                    IF (N.NE.-3 .OR. MTBTYP.EQ.1) THEN
                      VAL=0.0D0
                      DO 250 K=2,NUMK-1
                        NW=NF(I-IP,JC-JP,K)
                        IF     (NW.LE.-1) THEN
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
                    ELSE
                      VWS=1.0D0
                    ENDIF
C                   * 境界値を計算する
                    KMT=1
                    DO 270 K=2,NUMK-1
                      L=INDY(I-IP,J-JP,K)
                      IF (L.GE.1) THEN
                        ZC=(ZZ(1,K)+ZZ(1,K+1))*0.5D0-WVLVL
                        ZC=VWS*(ZC+D)-D
                        IF (N.NE.-3) THEN
                          CALL VF_CWMAK2(WVT,ZC,UN,UT,N)
                        ELSE
                          CALL VF_CWMTB2(KMT,WMT1,WMT2,ZC,UN,UT,
     &                                   DMTBZZ,DMTBUN,DMTBUT)
                        ENDIF
                        BCU(L,2)=UN*VWS*AW*SA
                        BCV(L,2)=UN*VWS*AW*SB
                        BCW(L,2)=UT*VWS*AW
                        CF1=BCF(L)
                        IF (CF1.GT.0.0D0) THEN
                          BCU(L,1)=BCU(L,2)
                          BCV(L,1)=BCV(L,2)
                          BCW(L,1)=BCW(L,2)
                        ELSE
                          CF2=1.0D0-CF1
                          BCU(L,1)=CF1*BCU(L,2)+CF2*BCU(L,3)
                          BCV(L,1)=CF1*BCV(L,2)+CF2*BCV(L,3)
                          BCW(L,1)=CF1*BCW(L,2)+CF2*BCW(L,3)
                        ENDIF
                        VV(I-IP,J-JP,K)=BCV(L,1)
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
C2F                   IF (NF(IC-IP,J-JP,K).EQ.0) THEN
                      IF (NF(IC-IP,J-JP,K).GE.0) THEN
                        VC=(VB(IC-IP,J-JP,K)+VB(IC-IP,J+1-JP,K))*0.5D0
                        WC=(WB(IC-IP,J-JP,K)+WB(IC-IP,J-JP,K+1))*0.5D0
                        BCU(L,1)=
     &                    (1.0D0-ALN)*BCU(L,1)+ALN*UB(I2-IP,J-JP,K)
                        BCV(L,1)=(1.0D0-ALT)*BCV(L,1)+ALT*VC
                        BCW(L,1)=(1.0D0-ALT)*BCW(L,1)+ALT*WC
                      ELSE
                        BCU(L,1)=0.0D0
                        BCV(L,1)=0.0D0
                        BCW(L,1)=0.0D0
                      ENDIF
                      BCU(L,2)=BCU(L,1)
                      BCV(L,2)=BCV(L,1)
                      BCW(L,2)=BCW(L,1)
                      BCU(L,3)=BCU(L,1)
                      BCV(L,3)=BCV(L,1)
                      BCW(L,3)=BCW(L,1)
                      UU(I-IP,J-JP,K)=BCU(L,1)
                    ENDIF
                  ENDIF
 300            CONTINUE
 310          CONTINUE
C             * 表面セルに接するセルに設定
C2F              DO 330 K=2,NUMK-1
C2F                DO 320 J=2,NUMJ0-1
C2F                  IF (MYGJS+1.LE.J .AND. J.LE.MYGJE-1) THEN
C2F                    L=INDX(I-IP,J-JP,K)
C2F                    IF (L.GE.1) THEN
C2F                      NW=NF(IC-IP,J-JP,K)
C2F                      IF     (NW.EQ.5) THEN
C2F                        L2=INDX(I-IP,J-JP,K-1)
C2F                        BCU(L)=BCU(L2)
C2F                        BCV(L)=BCV(L2)
C2F                        BCW(L)=BCW(L2)
C2F                        UU(I-IP,J-JP,K)=BCU(L)
C2F                      ELSEIF (NW.EQ.6) THEN
C2F                        L2=INDX(I-IP,J-JP,K+1)
C2F                        BCU(L)=BCU(L2)
C2F                        BCV(L)=BCV(L2)
C2F                        BCW(L)=BCW(L2)
C2F                        UU(I-IP,J-JP,K)=0.0D0
C2F                      ENDIF
C2F                    ENDIF
C2F                  ENDIF
C2F 320            CONTINUE
C2F 330          CONTINUE
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
C2F                   IF (NF(I-IP,JC-JP,K).EQ.0) THEN
                      IF (NF(I-IP,JC-JP,K).GE.0) THEN
                        UC=(UB(I-IP,JC-JP,K)+UB(I+1-IP,JC-JP,K))*0.5D0
                        WC=(WB(I-IP,JC-JP,K)+WB(I-IP,JC-JP,K+1))*0.5D0
                        BCU(L,1)=(1.0D0-ALT)*BCU(L,1)+ALT*UC
                        BCV(L,1)=(1.0D0-ALN)*BCV(L,1)
     &                              +ALN*VB(I-IP,J2-JP,K)
                        BCW(L,1)=(1.0D0-ALT)*BCW(L,1)+ALT*WC
                       ELSE
                        BCU(L,1)=0.0D0
                        BCV(L,1)=0.0D0
                        BCW(L,1)=0.0D0
                      ENDIF
                      BCU(L,2)=BCU(L,1)
                      BCV(L,2)=BCV(L,1)
                      BCW(L,2)=BCW(L,1)
                      BCU(L,3)=BCU(L,1)
                      BCV(L,3)=BCV(L,1)
                      BCW(L,3)=BCW(L,1)
                      VV(I-IP,J-JP,K)=BCV(L,1)
                    ENDIF
                  ENDIF
 400            CONTINUE
 410          CONTINUE
C             * 表面セルに接するセルに設定
C2F              DO 430 K=2,NUMK-1
C2F                DO 420 I=2,NUMI0-1
C2F                  IF (MYGIS+1.LE.I .AND. I.LE.MYGIE-1) THEN
C2F                    L=INDY(I-IP,J-JP,K)
C2F                    IF (L.GE.1) THEN
C2F                      NW=NF(I-IP,JC-JP,K)
C2F                      IF     (NW.EQ.5) THEN
C2F                        L2=INDY(I-IP,J-JP,K-1)
C2F                        BCU(L)=BCU(L2)
C2F                        BCV(L)=BCV(L2)
C2F                        BCW(L)=BCW(L2)
C2F                        VV(I-IP,J-JP,K)=BCV(L)
C2F                      ELSEIF (NW.EQ.6) THEN
C2F                        L2=INDY(I-IP,J-JP,K+1)
C2F                        BCU(L)=BCU(L2)
C2F                        BCV(L)=BCV(L2)
C2F                        BCW(L)=BCW(L2)
C2F                        VV(I-IP,J-JP,K)=BCV(L)
C2F                      ENDIF
C2F                    ENDIF
C2F                  ENDIF
C2F 420            CONTINUE
C2F 430          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 500  CONTINUE

      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
