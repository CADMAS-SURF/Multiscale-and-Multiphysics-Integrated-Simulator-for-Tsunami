      SUBROUTINE VF_CDTCAL(DT,XX,YY,ZZ,UU,VV,WW,FF,ANU,
     &                     GGV,GGX,GGY,GGZ,BCF,ALM,DD,WK01,
     &                     NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_CDTCAL:時間刻み幅の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    DT                : OUT : R*8 : 時間刻み幅
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)      : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)      : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)      : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    ANU(@FOR-3D@)     : IN  : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    BCF(NUMB)         : IN  : R*8 : VOF関数Fの境界値
CD    ALM(@FOR-3D@)     : IN  : R*8 : 熱伝導率と乱流熱伝導率の和
CD    DD(@FOR-3D@,LEQC) : IN  : R*8 : 拡散係数と乱流拡散係数の和
CD    WK01(@FOR-3D@)    : OUT : R*8 : ワーク配列
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),ANU (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION BCF (NUMB)
      DIMENSION ALM (NUMI,NUMJ,NUMK),DD  (NUMI,NUMJ,NUMK,LEQC)
      DIMENSION WK01(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 始めは最大値とする(安全率を考慮) --
      DT=DTMAX/DTSAFE

CD    -- 粘性等の最大値を求める --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            WK01(I,J,K)=ANU(I,J,K)
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE
      IF (LEQT.NE.0) THEN
        W=1.0D0/(RHO0*TCP0)
        DO 150 K=2,NUMK-1
          DO 140 J=MYJS,MYJE
            DO 130 I=MYIS,MYIE
              WK01(I,J,K)=MAX(WK01(I,J,K),ALM(I,J,K)*W)
 130        CONTINUE
 140      CONTINUE
 150    CONTINUE
      ENDIF
      DO 190 LC=1,LEQC
        DO 180 K=2,NUMK-1
          DO 170 J=MYJS,MYJE
            DO 160 I=MYIS,MYIE
              WK01(I,J,K)=MAX(WK01(I,J,K),DD(I,J,K,LC))
 160        CONTINUE
 170      CONTINUE
 180    CONTINUE
 190  CONTINUE

CD    -- 流速および粘性等による制限 --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            IF (NF(I,J,K).NE.-1) THEN

CD            -- 格子間隔と面積等 --
              DX =XX(2,I)
              DY =YY(2,J)
              DZ =ZZ(2,K)
              DXY=DX*DY
              DXZ=DX*DZ
              DYZ=DY*DZ
              FC =FF(I,J,K)

CD            -- 流速による制限 --
              V=GGV(I,J,K)*DX*DY*DZ
              FL1=ABS(DYZ*GGX(I  ,J,K)*UU(I  ,J,K))
              FL2=ABS(DYZ*GGX(I+1,J,K)*UU(I+1,J,K))
              FL3=ABS(DXZ*GGY(I,J  ,K)*VV(I,J  ,K))
              FL4=ABS(DXZ*GGY(I,J+1,K)*VV(I,J+1,K))
              FL5=ABS(DXY*GGZ(I,J,K  )*WW(I,J,K  ))
              FL6=ABS(DXY*GGZ(I,J,K+1)*WW(I,J,K+1))
              IF (INDX(I  ,J,K).GE.1) THEN
                IF (MAX(FC,BCF(INDX(I  ,J,K))).LE.0.0D0) FL1=0.0D0
              ENDIF
              IF (INDX(I+1,J,K).GE.1) THEN
                IF (MAX(FC,BCF(INDX(I+1,J,K))).LE.0.0D0) FL2=0.0D0
              ENDIF
              IF (INDY(I,J  ,K).GE.1) THEN
                IF (MAX(FC,BCF(INDY(I,J  ,K))).LE.0.0D0) FL3=0.0D0
              ENDIF
              IF (INDY(I,J+1,K).GE.1) THEN
                IF (MAX(FC,BCF(INDY(I,J+1,K))).LE.0.0D0) FL4=0.0D0
              ENDIF
              IF (INDZ(I,J,K  ).GE.1) THEN
                IF (MAX(FC,BCF(INDZ(I,J,K  ))).LE.0.0D0) FL5=0.0D0
              ENDIF
              IF (INDZ(I,J,K+1).GE.1) THEN
                IF (MAX(FC,BCF(INDZ(I,J,K+1))).LE.0.0D0) FL6=0.0D0
              ENDIF
              DT=V/MAX(FL1,FL2,FL3,FL4,FL5,FL6,V/DT)

CD            -- 粘性等による制限 --
              V=0.5D0/(1.0D0/(DX*DX)+1.0D0/(DY*DY)+1.0D0/(DZ*DZ))
              DT=V/MAX(WK01(I,J,K),V/DT)

            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- 安全率を乗じ、1.2倍以下と最小値による制限を加える --
      W=DT
      CALL VF_P0MIND(W,DT)
CAKIY CALL VF_P1MIND(W,DT)
      W=DTNOW
      IF (NNOW.EQ.0) W=DTINIT
      DT=MAX(MIN(DT*DTSAFE,1.20D0*W),DTMIN)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
