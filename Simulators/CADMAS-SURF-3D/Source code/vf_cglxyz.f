      SUBROUTINE VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                     NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_CGLXYZ:時間依存型のGLX,GLY,GLZを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    CM0(@FOR-3D@)  : IN  : R*8 : 慣性力係数
CD    GGX(@FOR-3D@)  : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)  : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)  : IN  : R*8 : z方向面積透過率
CD    GLX(@FOR-3D@)  : OUT : R*8 : =GGX+(1-GGX)*CM
CD    GLY(@FOR-3D@)  : OUT : R*8 : =GGY+(1-GGY)*CM
CD    GLZ(@FOR-3D@)  : OUT : R*8 : =GGZ+(1-GGZ)*CM
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@) : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@) : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@) : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION CM0 (NUMI,NUMJ,NUMK)
      DIMENSION GGX (NUMI,NUMJ,NUMK),GGY (NUMI,NUMJ,NUMK)
      DIMENSION GGZ (NUMI,NUMJ,NUMK),GLX (NUMI,NUMJ,NUMK)
      DIMENSION GLY (NUMI,NUMJ,NUMK),GLZ (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=1
      IF (MYMIE.EQ.1) IB=IB+1
      IF (MYMJS.EQ.1) JA=1
      IF (MYMJE.EQ.1) JB=JB+1

CD    -- GLX,GLY,GLZを計算 --
      DO 120 K=1,NUMK
        DO 110 J=JA,JB
          DO 100 I=IA,IB
C           * GLXを計算
            IF (INDX(I,J,K).EQ.-1) THEN
              GLX(I,J,K)=1.0D0
            ELSEIF (INDX(I,J,K).EQ.0) THEN
C             * (注)面積補間
              CM=XX(6,I)*(XX(2,I)*CM0(I,J,K)+XX(2,I-1)*CM0(I-1,J,K))
              GLX(I,J,K)=GGX(I,J,K)+(1.0D0-GGX(I,J,K))*CM
            ELSEIF (NF(I,J,K).NE.-1) THEN
              GLX(I,J,K)=GGX(I,J,K)+(1.0D0-GGX(I,J,K))*CM0(I  ,J,K)
            ELSE
              GLX(I,J,K)=GGX(I,J,K)+(1.0D0-GGX(I,J,K))*CM0(I-1,J,K)
            ENDIF
C           * GLYを計算
            IF (INDY(I,J,K).EQ.-1) THEN
              GLY(I,J,K)=1.0D0
            ELSEIF (INDY(I,J,K).EQ.0) THEN
C             * (注)面積補間
              CM=YY(6,J)*(YY(2,J)*CM0(I,J,K)+YY(2,J-1)*CM0(I,J-1,K))
              GLY(I,J,K)=GGY(I,J,K)+(1.0D0-GGY(I,J,K))*CM
            ELSEIF (NF(I,J,K).NE.-1) THEN
              GLY(I,J,K)=GGY(I,J,K)+(1.0D0-GGY(I,J,K))*CM0(I,J  ,K)
            ELSE
              GLY(I,J,K)=GGY(I,J,K)+(1.0D0-GGY(I,J,K))*CM0(I,J-1,K)
            ENDIF
C           * GLZを計算
            IF (INDZ(I,J,K).EQ.-1) THEN
              GLZ(I,J,K)=1.0D0
            ELSEIF (INDZ(I,J,K).EQ.0) THEN
C             * (注)面積補間
              CM=ZZ(6,K)*(ZZ(2,K)*CM0(I,J,K)+ZZ(2,K-1)*CM0(I,J,K-1))
              GLZ(I,J,K)=GGZ(I,J,K)+(1.0D0-GGZ(I,J,K))*CM
            ELSEIF (NF(I,J,K).NE.-1) THEN
              GLZ(I,J,K)=GGZ(I,J,K)+(1.0D0-GGZ(I,J,K))*CM0(I,J,K  )
            ELSE
              GLZ(I,J,K)=GGZ(I,J,K)+(1.0D0-GGZ(I,J,K))*CM0(I,J,K-1)
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD2(GLX,DBUF,1)
      CALL VF_P3SRD2(GLY,DBUF,2)
      CALL VF_P3SRD2(GLZ,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
