      SUBROUTINE VF_STOC_FCONV(XX,YY,UU,VV,FF,GGV,GGX,GGY,BCF,
     &                         FLFU,FLFV,NF,INDX,INDY)

CD=== 概要 ===========================================================

CDT   VF_STOC_FCONV:VOF関数Fの移流項によるフラックスの計算(STOC連成)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    FLFU(@FOR-3D@)   : I/O : R*8 : VOF関数Fのx方向フラックス
CD    FLFV(@FOR-3D@)   : I/O : R*8 : VOF関数Fのy方向フラックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),GGV (NUMI,NUMJ,NUMK)
      DIMENSION GGX (NUMI,NUMJ,NUMK),GGY (NUMI,NUMJ,NUMK)
      DIMENSION BCF(NUMB)
      DIMENSION FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK)
      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- dt*(Gx*u)*Fの計算 --
      DO 110 K=2,NUMK-1
        DO 100 J=MYJS,MYJE
          I=MYIS
          L=INDX(I,J,K)
          IF (L.GE.1 .AND. IWST.EQ.1) THEN
            C=DTNOW*GGX(I,J,K)*UU(I,J,K)
            FLFU(I,J,K)=-BCF(L)*C
          ENDIF
          I=MYIE+1
          L=INDX(I,J,K)
          IF (L.GE.1 .AND. IEST.EQ.1) THEN
            C=DTNOW*GGX(I,J,K)*UU(I,J,K)
            FLFU(I,J,K)=-BCF(L)*C
          ENDIF
 100    CONTINUE
 110  CONTINUE

CD    -- dt*(Gy*v)*Fの計算 --
      DO 210 K=2,NUMK-1
        DO 200 I=MYIS,MYIE
          J=MYJS
          L=INDY(I,J,K)
          IF (L.GE.1 .AND. JSST.EQ.1) THEN
            C=DTNOW*GGY(I,J,K)*VV(I,J,K)
            FLFV(I,J,K)=-BCF(L)*C
          ENDIF
          J=MYJE+1
          L=INDY(I,J,K)
          IF (L.GE.1 .AND. JNST.EQ.1) THEN
            C=DTNOW*GGY(I,J,K)*VV(I,J,K)
            FLFV(I,J,K)=-BCF(L)*C
          ENDIF
 200    CONTINUE
 210  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
