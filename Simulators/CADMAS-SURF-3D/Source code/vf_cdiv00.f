      SUBROUTINE VF_CDIV00(VAL,XX,YY,ZZ,UU,VV,WW,GGX,GGY,GGZ,INDC)

CD=== 概要 ===========================================================

CDT   VF_CDIV00:(発散×面積)の2乗ノルムを計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    VAL               : OUT : R*8 : (発散*面積)の2乗ノルム
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)      : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)      : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)      : IN  : R*8 : z方向流速
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    INDC(@FOR-3D@)    : IN  : I*4 : セルの計算状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- (発散*面積)の2乗ノルム --
      VAL=0.0D0
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
              VD= YY(2,J)*ZZ(2,K)*( GGX(I+1,J,K)*UU(I+1,J,K)
     &                             -GGX(I  ,J,K)*UU(I  ,J,K))
     &           +XX(2,I)*ZZ(2,K)*( GGY(I,J+1,K)*VV(I,J+1,K)
     &                             -GGY(I,J  ,K)*VV(I,J  ,K))
     &           +XX(2,I)*YY(2,J)*( GGZ(I,J,K+1)*WW(I,J,K+1)
     &                             -GGZ(I,J,K  )*WW(I,J,K  ))
              VAL=VAL+VD*VD
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE
      W=VAL
      CALL VF_P1SUMD(W,VAL)
      VAL=SQRT(VAL)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
