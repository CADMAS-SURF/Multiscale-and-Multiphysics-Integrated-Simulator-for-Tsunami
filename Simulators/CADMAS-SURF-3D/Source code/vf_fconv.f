      SUBROUTINE VF_FCONV(XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,BCF,
     &                    FLFU,FLFV,FLFW,NF,INDX,INDY,INDZ,NLIM)

CD=== 概要 ===========================================================

CDT   VF_FCONV:VOF関数Fの移流項によるフラックスの計算(ドナーアクセプタ)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    FLFU(@FOR-3D@)   : I/O : R*8 : VOF関数Fのx方向フラックス
CD    FLFV(@FOR-3D@)   : I/O : R*8 : VOF関数Fのy方向フラックス
CD    FLFW(@FOR-3D@)   : I/O : R*8 : VOF関数Fのz方向フラックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    NLIM(@FOR-3D@)   : I/O : I*4 : 補正を行うためのワーク
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION BCF(NUMB)
      DIMENSION FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)
      DIMENSION FLFW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION NLIM(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- dt*(Gx*u)*Fの計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE+1
            IF (INDX(I,J,K).NE.-1) THEN
              C=DTNOW*GGX(I,J,K)*UU(I,J,K)
              AC=ABS(C)
              IF (C.GE.0.0D0) THEN
                LSN=1
                LA =I
                LD =I-1
                LDM=I-2
              ELSE
                LSN=-1
                LA =I-1
                LD =I
                LDM=I+1
              ENDIF
              N=NF(LD,J,K)
              IF (N.NE.-1) THEN
                FD=FF(LD,J,K)
                FDMW=1.0D0
                IF (NF(LDM,J,K).NE.-1) FDMW=FF(LDM,J,K)
                V=XX(2,LD)*GGV(LD,J,K)
              ELSE
                FD=BCF(INDX(I,J,K))
                FDMW=1.0D0
                V=XX(2,LD)
              ENDIF
              IF (NF(LA,J,K).NE.-1) THEN
                FA=FF(LA,J,K)
              ELSE
                FA=FD
              ENDIF
              FAD=FA
              IF (N.EQ.-1 .OR. N.EQ.3 .OR. N.EQ.4
     &                    .OR. N.EQ.5 .OR. N.EQ.6) THEN
                IF (FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD=FD
              ENDIF
              FDM=MAX(FDMW,FD)
              IF (FDMW.LT.FLOWER .AND. FA.LT.FLOWER) FDM=MAX(FDM,0.1D0)
              CFX=MAX((FDM-FAD)*AC-(FDM-FD)*V,0.0D0)
              FVX=FAD*AC+CFX
              FVM=FD*V
              FLFU(I,J,K)=FLFU(I,J,K)-DBLE(LSN)*MIN(FVX,FVM)
              IF (FVX.GT.FVM) NLIM(LD,J,K)=1
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- dt*(Gy*v)*Fの計算 --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE+1
          DO 200 I=MYIS,MYIE
            IF (INDY(I,J,K).NE.-1) THEN
              C=DTNOW*GGY(I,J,K)*VV(I,J,K)
              AC=ABS(C)
              IF (C.GE.0.0D0) THEN
                LSN=1
                LA =J
                LD =J-1
                LDM=J-2
              ELSE
                LSN=-1
                LA =J-1
                LD =J
                LDM=J+1
              ENDIF
              N=NF(I,LD,K)
              IF (N.NE.-1) THEN
                FD=FF(I,LD,K)
                FDMW=1.0D0
                IF (NF(I,LDM,K).NE.-1) FDMW=FF(I,LDM,K)
                V=YY(2,LD)*GGV(I,LD,K)
              ELSE
                FD=BCF(INDY(I,J,K))
                FDMW=1.0D0
                V=YY(2,LD)
              ENDIF
              IF (NF(I,LA,K).NE.-1) THEN
                FA=FF(I,LA,K)
              ELSE
                FA=FD
              ENDIF
              FAD=FA
              IF (N.EQ.-1 .OR. N.EQ.1 .OR. N.EQ.2
     &                    .OR. N.EQ.5 .OR. N.EQ.6) THEN
                IF (FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD=FD
              ENDIF
              FDM=MAX(FDMW,FD)
              IF (FDMW.LT.FLOWER .AND. FA.LT.FLOWER) FDM=MAX(FDM,0.1D0)
              CFX=MAX((FDM-FAD)*AC-(FDM-FD)*V,0.0D0)
              FVX=FAD*AC+CFX
              FVM=FD*V
              FLFV(I,J,K)=FLFV(I,J,K)-DBLE(LSN)*MIN(FVX,FVM)
              IF (FVX.GT.FVM) NLIM(I,LD,K)=1
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- dt*(Gz*w)*Fの計算 --
      DO 320 K=2,NUMK
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (INDZ(I,J,K).NE.-1) THEN
              C=DTNOW*GGZ(I,J,K)*WW(I,J,K)
              AC=ABS(C)
              IF (C.GE.0.0D0) THEN
                LSN=1
                LA =K
                LD =K-1
                LDM=K-2
              ELSE
                LSN=-1
                LA =K-1
                LD =K
                LDM=K+1
              ENDIF
              N=NF(I,J,LD)
              IF (N.NE.-1) THEN
                FD=FF(I,J,LD)
                FDMW=1.0D0
                IF (NF(I,J,LDM).NE.-1) FDMW=FF(I,J,LDM)
                V=ZZ(2,LD)*GGV(I,J,LD)
              ELSE
                FD=BCF(INDZ(I,J,K))
                FDMW=1.0D0
                V=ZZ(2,LD)
              ENDIF
              IF (NF(I,J,LA).NE.-1) THEN
                FA=FF(I,J,LA)
              ELSE
                FA=FD
              ENDIF
              FAD=FA
              IF (N.EQ.-1 .OR. N.EQ.1 .OR. N.EQ.2
     &                    .OR. N.EQ.3 .OR. N.EQ.4) THEN
                IF (FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD=FD
              ENDIF
              FDM=MAX(FDMW,FD)
              IF (FDMW.LT.FLOWER .AND. FA.LT.FLOWER) FDM=MAX(FDM,0.1D0)
              CFX=MAX((FDM-FAD)*AC-(FDM-FD)*V,0.0D0)
              FVX=FAD*AC+CFX
              FVM=FD*V
              FLFW(I,J,K)=FLFW(I,J,K)-DBLE(LSN)*MIN(FVX,FVM)
              IF (FVX.GT.FVM) NLIM(I,J,LD)=1
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
