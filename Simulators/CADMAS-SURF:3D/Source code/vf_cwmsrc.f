      SUBROUTINE VF_CWMSRC(TDMY,ZZ,FF,SRCUV,
     &                     DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,
     &                     NF)

CD=== 概要 ===========================================================

CDT   VF_CWMSRC:造波ソースのための流速を計算する

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
CD    TDMY              : IN  : R*8 : 対象とする時刻
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    SRCUV(NUMIJ,NUMK) : OUT : R*8 : 造波ソースのための流速
CD    DMTBTT(MTBTT)       : IN : R*8 : マトリクスデータの無次元位相
CD    DMTBZZ(MTBZZ)       : IN : R*8 : マトリクスデータのz座標
CD    DMTBHH(MTBTT)       : IN : R*8 : マトリクスデータの水位
CD    DMTBUN(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの鉛直方向流速
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION ZZ(MAXG1,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK),SRCUV(NUMIJ,NUMK)
      DIMENSION DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
      DIMENSION DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C     -- 局所変数 --
      REAL*4 R4TN

C==== 実行 ===========================================================

CD    -- 並列時のシフトと判定(通信をしないために範囲は広くする) --
      IF     (ISCTYP(1).GT.0) THEN
C        IJ1= MYJS
C        IJ2= MYJE
C        IJP= ISCTYP(1)-(MYGIS-1)
C        IF (MYIS.GT.IJP .OR. IJP.GT.MYIE) IJP=0
        IJ1= 2
        IJ2= NUMJ-1
        IJP= ISCTYP(1)-(MYGIS-1)
        IF (2.GT.IJP .OR. IJP.GT.NUMI-1) IJP=0
      ELSEIF (ISCTYP(1).LT.0) THEN
C        IJ1= MYIS
C        IJ2= MYIE
C        IJP=-ISCTYP(1)-(MYGJS-1)
C        IF (MYJS.GT.IJP .OR. IJP.GT.MYJE) IJP=0
        IJ1= 2
        IJ2= NUMI-1
        IJP=-ISCTYP(1)-(MYGJS-1)
        IF (2.GT.IJP .OR. IJP.GT.NUMJ-1) IJP=0
      ELSE
        CALL VF_A2ERR('VF_CWMSRC','P.G ERROR.')
      ENDIF

CD    -- 期待する水位を計算 --
      N =ISCTYP(2)
      D =SCTYP (1)
      H =SCTYP (2)
      T =SCTYP (3)
      DL=SCTYP (4)
      T0=SCTYP (6)
      A =SCTYP (8)
C     * 造波ソースのための無次元位相の計算
C       CADMAS-SURFとの結果が変わらないよう、倍精度は別に計算
      TN=T0-TDMY/T
      TN=TN-DBLE(INT(TN))
      IF (TN.LT.0.0D0) TN=TN+1.0D0
      R4TN=REAL(T0-TDMY/T)
      R4TN=R4TN-REAL(INT(R4TN))
      IF (R4TN.LT.0.0E0) R4TN=R4TN+1.0E0
C     * 増幅率の計算
      AW=1.0D0
      IF (A.GE.ZERO) THEN
        A=TDMY/T/A
        IF (A.LT.1.0D0) AW=0.5D0*SIN(PI*(A-0.5D0))+0.5D0
      ENDIF
C     * 造波のための関数を初期化する
      CALL VF_CWMAK0(D,T,H,WLN,N)
C     * 期待する水位を計算する
      WVT=DBLE(R4TN)
      IF (N.NE.-3) THEN
        CALL VF_CWMAK1(WVT,WVZ,N)
      ELSE
        CALL VF_CWMTB1(WVT,WMT1,WMT2,WVZ,DMTBTT,DMTBHH)
      ENDIF
      SCTYP(7)=WVZ*AW

CD    -- 範囲内ならば計算 --
      IF (IJP.NE.0) THEN
        DO 200 IJ=IJ1,IJ2
          IF (ISCTYP(1).GT.0) THEN
            I=IJP
            J=IJ
          ELSE
            I=IJ
            J=IJP
          ENDIF
C         * 現在の水位を計算し、スケーリングを決める
          IF (N.NE.-3 .OR. MTBTYP.EQ.1) THEN
            VAL=0.0D0
            DO 100 K=2,NUMK-1
              NW=NF(I,J,K)
              IF     (NW.EQ.-1) THEN
                VAL=VAL+ZZ(2,K)*1.0D0
              ELSEIF (NW.EQ. 0) THEN
                VAL=VAL+ZZ(2,K)*FF(I,J,K)
              ELSEIF (NW.NE. 8) THEN
                VAL=VAL+ZZ(2,K)*FF(I,J,K)
                GOTO 110
              ENDIF
 100        CONTINUE
 110        CONTINUE
            VAL=VAL-(WVLVL-ZZ(1,2))
            VWS=(WVZ+D)/(VAL+D)
          ELSE
            VWS=1.0D0
          ENDIF
C         * 造波ソースを計算する
          KMT=1
          DO 120 K=2,NUMK-1
            ZC=(ZZ(1,K)+ZZ(1,K+1))*0.5D0-WVLVL
            ZC=VWS*(ZC+D)-D
            IF (N.NE.-3) THEN
              CALL VF_CWMAK2(WVT,ZC,UN,UT,N)
            ELSE
              CALL VF_CWMTB2(KMT,WMT1,WMT2,ZC,UN,UT,
     &                       DMTBZZ,DMTBUN,DMTBUT)
            ENDIF
            SRCUV(IJ,K)=UN*VWS*AW
 120      CONTINUE
 200    CONTINUE
      ENDIF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
