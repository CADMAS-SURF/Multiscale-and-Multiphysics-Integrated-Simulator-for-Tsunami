      SUBROUTINE VF_FCONVS(XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,BCF,
     &                     DBUF,FLFU,FLFV,FLFW,
     &                     NF,INDX,INDY,INDZ,IBUF,ISTYP,
     &                     SX,SY,SZ,SA)

CD=== 概要 ===========================================================

CDT   VF_FCONVS:VOF関数Fの移流項によるフラックスの計算(傾斜を考慮)

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
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLFU(@FOR-3D@)   : I/O : R*8 : VOF関数Fのx方向フラックス
CD    FLFV(@FOR-3D@)   : I/O : R*8 : VOF関数Fのy方向フラックス
CD    FLFW(@FOR-3D@)   : I/O : R*8 : VOF関数Fのz方向フラックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
CD    ISTYP(@FOR-3D@)  : OUT : I*4 : 斜面のタイプ
CD                                   = 0:斜面無し(ドナーアクセプタ使用)
CD                                   = 3:(SX,SY,0 )
CD                                   = 5:(SX,0 ,SZ)
CD                                   = 6:(0 ,SY,SZ)
CD                                   = 7:(SX,SY,SZ)
CD    SX(@FOR-3D@)     : OUT : R*8 : 斜面の法線ベクトル(x方向)
CD    SY(@FOR-3D@)     : OUT : R*8 : 斜面の法線ベクトル(y方向)
CD    SZ(@FOR-3D@)     : OUT : R*8 : 斜面の法線ベクトル(z方向)
CD    SA(@FOR-3D@)     : OUT : R*8 : 斜面の一次方程式の右辺
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION BCF(NUMB)           ,DBUF(NUMBUF*MAXBUF)
      DIMENSION FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)
      DIMENSION FLFW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION IBUF(NUMBUF*MAXBUF) ,ISTYP(NUMI,NUMJ,NUMK)
      DIMENSION SX  (NUMI,NUMJ,NUMK),SY  (NUMI,NUMJ,NUMK)
      DIMENSION SZ  (NUMI,NUMJ,NUMK),SA  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- まず、斜面のタイプを斜面無しとする --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            ISTYP(I,J,K)=0
            SX   (I,J,K)=0.0D0
            SY   (I,J,K)=0.0D0
            SZ   (I,J,K)=0.0D0
            SA   (I,J,K)=0.0D0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 法線ベクトルの算出と界面の決定 --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            NF0=NF(I,J,K)
            FF0=FF(I,J,K)
C           * 表面セルか気体セルで、かつ、EPS<FF<1.0-EPSのセルについて
            IF (NF0.GT.0 .AND. FF0.GE.FLOWER .AND. FF0.LE.FUPPER) THEN

C             * I-1側
              DD0=XX(2,I)
              L0 =I-1
              NFL=NF(L0,J,K)
              FFL=FF(L0,J,K)
              IF     (NFL.EQ.-1) THEN
                FM=BCF(INDX(I,J,K))
                DM=DD0*0.5D0
              ELSEIF (NFL.EQ. 8) THEN
                IF (NF0.EQ.3 .OR. NF0.EQ.4 .OR.
     &              NF0.EQ.5 .OR. NF0.EQ.6     ) THEN
                  FM=FF0
                  DM=0.0D0
                ELSE
                  FM=0.0D0
                  DM=DD0*0.5D0
                ENDIF
              ELSEIF (NFL.EQ. 0) THEN
                FM=FFL
                DM=DD0*0.5D0
                IF     (NF0.EQ.3 .AND. NF(L0,J+1,K).NE.-1) THEN
                  FM=FM+FF(L0,J+1,K)
                  DM=XX(3,I)
                ELSEIF (NF0.EQ.4 .AND. NF(L0,J-1,K).NE.-1) THEN
                  FM=FM+FF(L0,J-1,K)
                  DM=XX(3,I)
                ELSEIF (NF0.EQ.5 .AND. NF(L0,J,K+1).NE.-1) THEN
                  FM=FM+FF(L0,J,K+1)
                  DM=XX(3,I)
                ELSEIF (NF0.EQ.6 .AND. NF(L0,J,K-1).NE.-1) THEN
                  FM=FM+FF(L0,J,K-1)
                  DM=XX(3,I)
                ENDIF
              ELSE
                FM=XX(6,I)*(XX(2,L0)*FF0+DD0*FFL)
                DM=DD0*0.5D0
              ENDIF

C             * I+1側
              DD0=XX(2,I)
              L0 =I+1
              NFL=NF(L0,J,K)
              FFL=FF(L0,J,K)
              IF     (NFL.EQ.-1) THEN
                FP=BCF(INDX(L0,J,K))
                DP=DD0*0.5D0
              ELSEIF (NFL.EQ. 8) THEN
                IF (NF0.EQ.3 .OR. NF0.EQ.4 .OR.
     &              NF0.EQ.5 .OR. NF0.EQ.6     ) THEN
                  FP=FF0
                  DP=0.0D0
                ELSE
                  FP=0.0D0
                  DP=DD0*0.5D0
                ENDIF
              ELSEIF (NFL.EQ. 0) THEN
                FP=FFL
                DP=DD0*0.5D0
                IF     (NF0.EQ.3 .AND. NF(L0,J+1,K).NE.-1) THEN
                  FP=FP+FF(L0,J+1,K)
                  DP=XX(3,L0)
                ELSEIF (NF0.EQ.4 .AND. NF(L0,J-1,K).NE.-1) THEN
                  FP=FP+FF(L0,J-1,K)
                  DP=XX(3,L0)
                ELSEIF (NF0.EQ.5 .AND. NF(L0,J,K+1).NE.-1) THEN
                  FP=FP+FF(L0,J,K+1)
                  DP=XX(3,L0)
                ELSEIF (NF0.EQ.6 .AND. NF(L0,J,K-1).NE.-1) THEN
                  FP=FP+FF(L0,J,K-1)
                  DP=XX(3,L0)
                ENDIF
              ELSE
                FP=XX(6,L0)*(XX(2,L0)*FF0+DD0*FFL)
                DP=DD0*0.5D0
              ENDIF

C             * 法線ベクトルのx成分
              IF (DM+DP.LE.ZERO) THEN
                S1=0.0D0
              ELSE
                S1=(FM-FP)/(DM+DP)
              ENDIF

C             * J-1側
              DD0=YY(2,J)
              L0 =J-1
              NFL=NF(I,L0,K)
              FFL=FF(I,L0,K)
              IF     (NFL.EQ.-1) THEN
                FM=BCF(INDY(I,J,K))
                DM=DD0*0.5D0
              ELSEIF (NFL.EQ. 8) THEN
                IF (NF0.EQ.1 .OR. NF0.EQ.2 .OR.
     &              NF0.EQ.5 .OR. NF0.EQ.6     ) THEN
                  FM=FF0
                  DM=0.0D0
                ELSE
                  FM=0.0D0
                  DM=DD0*0.5D0
                ENDIF
              ELSEIF (NFL.EQ. 0) THEN
                FM=FFL
                DM=DD0*0.5D0
                IF     (NF0.EQ.1 .AND. NF(I+1,L0,K).NE.-1) THEN
                  FM=FM+FF(I+1,L0,K)
                  DM=YY(3,J)
                ELSEIF (NF0.EQ.2 .AND. NF(I-1,L0,K).NE.-1) THEN
                  FM=FM+FF(I-1,L0,K)
                  DM=YY(3,J)
                ELSEIF (NF0.EQ.5 .AND. NF(I,L0,K+1).NE.-1) THEN
                  FM=FM+FF(I,L0,K+1)
                  DM=YY(3,J)
                ELSEIF (NF0.EQ.6 .AND. NF(I,L0,K-1).NE.-1) THEN
                  FM=FM+FF(I,L0,K-1)
                  DM=YY(3,J)
                ENDIF
              ELSE
                FM=YY(6,J)*(YY(2,L0)*FF0+DD0*FFL)
                DM=DD0*0.5D0
              ENDIF

C             * J+1側
              DD0=YY(2,J)
              L0 =J+1
              NFL=NF(I,L0,K)
              FFL=FF(I,L0,K)
              IF     (NFL.EQ.-1) THEN
                FP=BCF(INDY(I,L0,K))
                DP=DD0*0.5D0
              ELSEIF (NFL.EQ. 8) THEN
                IF (NF0.EQ.1 .OR. NF0.EQ.2 .OR.
     &              NF0.EQ.5 .OR. NF0.EQ.6     ) THEN
                  FP=FF0
                  DP=0.0D0
                ELSE
                  FP=0.0D0
                  DP=DD0*0.5D0
                ENDIF
              ELSEIF (NFL.EQ. 0) THEN
                FP=FFL
                DP=DD0*0.5D0
                IF     (NF0.EQ.1 .AND. NF(I+1,L0,K).NE.-1) THEN
                  FP=FP+FF(I+1,L0,K)
                  DP=YY(3,L0)
                ELSEIF (NF0.EQ.2 .AND. NF(I-1,L0,K).NE.-1) THEN
                  FP=FP+FF(I-1,L0,K)
                  DP=YY(3,L0)
                ELSEIF (NF0.EQ.5 .AND. NF(I,L0,K+1).NE.-1) THEN
                  FP=FP+FF(I,L0,K+1)
                  DP=YY(3,L0)
                ELSEIF (NF0.EQ.6 .AND. NF(I,L0,K-1).NE.-1) THEN
                  FP=FP+FF(I,L0,K-1)
                  DP=YY(3,L0)
                ENDIF
              ELSE
                FP=YY(6,L0)*(YY(2,L0)*FF0+DD0*FFL)
                DP=DD0*0.5D0
              ENDIF

C             * 法線ベクトルのy成分
              IF (DM+DP.LE.ZERO) THEN
                S2=0.0D0
              ELSE
                S2=(FM-FP)/(DM+DP)
              ENDIF

C             * K-1側
              DD0=ZZ(2,K)
              L0 =K-1
              NFL=NF(I,J,L0)
              FFL=FF(I,J,L0)
              IF     (NFL.EQ.-1) THEN
                FM=BCF(INDZ(I,J,K))
                DM=DD0*0.5D0
              ELSEIF (NFL.EQ. 8) THEN
                IF (NF0.EQ.1 .OR. NF0.EQ.2 .OR.
     &              NF0.EQ.3 .OR. NF0.EQ.4     ) THEN
                  FM=FF0
                  DM=0.0D0
                ELSE
                  FM=0.0D0
                  DM=DD0*0.5D0
                ENDIF
              ELSEIF (NFL.EQ. 0) THEN
                FM=FFL
                DM=DD0*0.5D0
                IF     (NF0.EQ.1 .AND. NF(I+1,J,L0).NE.-1) THEN
                  FM=FM+FF(I+1,J,L0)
                  DM=ZZ(3,K)
                ELSEIF (NF0.EQ.2 .AND. NF(I-1,J,L0).NE.-1) THEN
                  FM=FM+FF(I-1,J,L0)
                  DM=ZZ(3,K)
                ELSEIF (NF0.EQ.3 .AND. NF(I,J+1,L0).NE.-1) THEN
                  FM=FM+FF(I,J+1,L0)
                  DM=ZZ(3,K)
                ELSEIF (NF0.EQ.4 .AND. NF(I,J-1,L0).NE.-1) THEN
                  FM=FM+FF(I,J-1,L0)
                  DM=ZZ(3,K)
                ENDIF
              ELSE
                FM=ZZ(6,K)*(ZZ(2,L0)*FF0+DD0*FFL)
                DM=DD0*0.5D0
              ENDIF

C             * K+1側
              DD0=ZZ(2,K)
              L0 =K+1
              NFL=NF(I,J,L0)
              FFL=FF(I,J,L0)
              IF     (NFL.EQ.-1) THEN
                FP=BCF(INDZ(I,J,L0))
                DP=DD0*0.5D0
              ELSEIF (NFL.EQ. 8) THEN
                IF (NF0.EQ.1 .OR. NF0.EQ.2 .OR.
     &              NF0.EQ.3 .OR. NF0.EQ.4     ) THEN
                  FP=FF0
                  DP=0.0D0
                ELSE
                  FP=0.0D0
                  DP=DD0*0.5D0
                ENDIF
              ELSEIF (NFL.EQ. 0) THEN
                FP=FFL
                DP=DD0*0.5D0
                IF     (NF0.EQ.1 .AND. NF(I+1,J,L0).NE.-1) THEN
                  FP=FP+FF(I+1,J,L0)
                  DP=ZZ(3,L0)
                ELSEIF (NF0.EQ.2 .AND. NF(I-1,J,L0).NE.-1) THEN
                  FP=FP+FF(I-1,J,L0)
                  DP=ZZ(3,L0)
                ELSEIF (NF0.EQ.3 .AND. NF(I,J+1,L0).NE.-1) THEN
                  FP=FP+FF(I,J+1,L0)
                  DP=ZZ(3,L0)
                ELSEIF (NF0.EQ.4 .AND. NF(I,J-1,L0).NE.-1) THEN
                  FP=FP+FF(I,J-1,L0)
                  DP=ZZ(3,L0)
                ENDIF
              ELSE
                FP=ZZ(6,L0)*(ZZ(2,L0)*FF0+DD0*FFL)
                DP=DD0*0.5D0
              ENDIF

C             * 法線ベクトルのz成分
              IF (DM+DP.LE.ZERO) THEN
                S3=0.0D0
              ELSE
                S3=(FM-FP)/(DM+DP)
              ENDIF

C             * 斜面の有無を判定し、法線ベクトルを単位長さにする
              RN=S1*S1+S2*S2+S3*S3
              IF (RN.GE.ZERO) THEN
                RN=1.0D0/SQRT(RN)
                S1=S1*RN
                S2=S2*RN
                S3=S3*RN
                I1=0
                I2=0
                I3=0
                IF (ABS(S1).GE.1.0D-4) I1=1
                IF (ABS(S2).GE.1.0D-4) I2=1
                IF (ABS(S3).GE.1.0D-4) I3=1
                IF (I1+I2+I3.GE.2) THEN
                  ISTYP(I,J,K)=I1+2*I2+4*I3
                  SX(I,J,K)=S1
                  SY(I,J,K)=S2
                  SZ(I,J,K)=S3
                ENDIF
              ENDIF

C             * 界面の決定(斜面の一次方程式の右辺の決定)
              ISW=ISTYP(I,J,K)
              IF     (ISW.EQ.0) THEN
              ELSEIF (ISW.EQ.3) THEN
                CALL VF_FSLP2A(SX(I,J,K),SY(I,J,K),
     &                         XX(2,I)  ,YY(2,J)  ,
     &                         FF(I,J,K),SA(I,J,K) )
              ELSEIF (ISW.EQ.5) THEN
                CALL VF_FSLP2A(SX(I,J,K),SZ(I,J,K),
     &                         XX(2,I)  ,ZZ(2,K)  ,
     &                         FF(I,J,K),SA(I,J,K) )
              ELSEIF (ISW.EQ.6) THEN
                CALL VF_FSLP2A(SY(I,J,K),SZ(I,J,K),
     &                         YY(2,J)  ,ZZ(2,K)  ,
     &                         FF(I,J,K),SA(I,J,K) )
              ELSEIF (ISW.EQ.7) THEN
                CALL VF_FSLP3A(SX(I,J,K),SY(I,J,K),SZ(I,J,K),
     &                         XX(2,I)  ,YY(2,J)  ,ZZ(2,K)  ,
     &                         FF(I,J,K),SA(I,J,K)           )
              ELSE
                CALL VF_A2ERR('VF_FCONVS','P.G ERROR(1).')
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

      CALL VF_P3SRI1(ISTYP,IBUF,0)
      CALL VF_P3SRD1(SX   ,DBUF,0)
      CALL VF_P3SRD1(SY   ,DBUF,0)
      CALL VF_P3SRD1(SZ   ,DBUF,0)
      CALL VF_P3SRD1(SA   ,DBUF,0)

CD    -- 移流量の算出 --

C     * dt*(Gx*u)*Fの計算
      DO 420 K=2,NUMK-1
        DO 410 J=MYJS,MYJE
          DO 400 I=MYIS,MYIE+1
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
              IF (INDX(I,J,K).GT.1 .OR. ISTYP(LD,J,K).LE.0
     &                             .OR. ISTYP(LD,J,K).EQ.6) THEN
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
     &                      .OR. N.EQ.5 .OR. N.EQ.6) THEN
                  IF (FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD=FD
                ENDIF
                FDM=MAX(FDMW,FD)
                IF (FDMW.LT.FLOWER .AND. FA.LT.FLOWER)
     &                                      FDM=MAX(FDM,0.1D0)
                CFX=MAX((FDM-FAD)*AC-(FDM-FD)*V,0.0D0)
                FVX=FAD*AC+CFX
                FVM=FD*V
                FLFU(I,J,K)=FLFU(I,J,K)-DBLE(LSN)*MIN(FVX,FVM)
              ELSE
                LSW=LSN
                IF (SX(LD,J,K).LT.0.0D0) LSW=-LSW
                DD0=AC
                IF (LSW.EQ.1) DD0=XX(2,LD)-AC
                ISW=ISTYP(LD,J,K)
                IF     (ISW.EQ.3) THEN
                  CALL VF_FSLP2F(SX(LD,J,K),SY(LD,J,K),
     &                           DD0       ,YY(2,J)   ,
     &                           SA(LD,J,K),DFF        )
                ELSEIF (ISW.EQ.5) THEN
                  CALL VF_FSLP2F(SX(LD,J,K),SZ(LD,J,K),
     &                           DD0       ,ZZ(2,K)   ,
     &                           SA(LD,J,K),DFF        )
                ELSEIF (ISW.EQ.7) THEN
                  CALL VF_FSLP3F(SX(LD,J,K),SY(LD,J,K),SZ(LD,J,K),
     &                           DD0       ,YY(2,J)   ,ZZ(2,K)   ,
     &                           SA(LD,J,K),DFF                   )
                ELSE
                  CALL VF_A2ERR('VF_FCONVS','P.G ERROR(2).')
                ENDIF
                VG=XX(2,LD)*FF(LD,J,K)
                FVX=DFF*DD0
                IF (LSW.EQ.1) FVX=VG-FVX
                VG=VG*GGV(LD,J,K)
                FLFU(I,J,K)=FLFU(I,J,K)-DBLE(LSN)*MIN(FVX,VG)
              ENDIF
            ENDIF
 400      CONTINUE
 410    CONTINUE
 420  CONTINUE

C     * dt*(Gy*v)*Fの計算
      DO 520 K=2,NUMK-1
        DO 510 J=MYJS,MYJE+1
          DO 500 I=MYIS,MYIE
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
              IF (INDY(I,J,K).GT.1 .OR. ISTYP(I,LD,K).LE.0
     &                             .OR. ISTYP(I,LD,K).EQ.5) THEN
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
     &                      .OR. N.EQ.5 .OR. N.EQ.6) THEN
                  IF (FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD=FD
                ENDIF
                FDM=MAX(FDMW,FD)
                IF (FDMW.LT.FLOWER .AND. FA.LT.FLOWER)
     &                                      FDM=MAX(FDM,0.1D0)
                CFX=MAX((FDM-FAD)*AC-(FDM-FD)*V,0.0D0)
                FVX=FAD*AC+CFX
                FVM=FD*V
                FLFV(I,J,K)=FLFV(I,J,K)-DBLE(LSN)*MIN(FVX,FVM)
              ELSE
                LSW=LSN
                IF (SY(I,LD,K).LT.0.0D0) LSW=-LSW
                DD0=AC
                IF (LSW.EQ.1) DD0=YY(2,LD)-AC
                ISW=ISTYP(I,LD,K)
                IF     (ISW.EQ.3) THEN
                  CALL VF_FSLP2F(SX(I,LD,K),SY(I,LD,K),
     &                           XX(2,I)   ,DD0       ,
     &                           SA(I,LD,K),DFF        )
                ELSEIF (ISW.EQ.6) THEN
                  CALL VF_FSLP2F(SY(I,LD,K),SZ(I,LD,K),
     &                           DD0       ,ZZ(2,K)   ,
     &                           SA(I,LD,K),DFF        )
                ELSEIF (ISW.EQ.7) THEN
                  CALL VF_FSLP3F(SX(I,LD,K),SY(I,LD,K),SZ(I,LD,K),
     &                           XX(2,I)   ,DD0       ,ZZ(2,K)   ,
     &                           SA(I,LD,K),DFF                   )
                ELSE
                  CALL VF_A2ERR('VF_FCONVS','P.G ERROR(3).')
                ENDIF
                VG=YY(2,LD)*FF(I,LD,K)
                FVX=DFF*DD0
                IF (LSW.EQ.1) FVX=VG-FVX
                VG=VG*GGV(I,LD,K)
                FLFV(I,J,K)=FLFV(I,J,K)-DBLE(LSN)*MIN(FVX,VG)
              ENDIF
            ENDIF
 500      CONTINUE
 510    CONTINUE
 520  CONTINUE

C     * dt*(Gz*w)*Fの計算
      DO 620 K=2,NUMK
        DO 610 J=MYJS,MYJE
          DO 600 I=MYIS,MYIE
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
              IF (INDZ(I,J,K).GT.1 .OR. ISTYP(I,J,LD).LE.0
     &                             .OR. ISTYP(I,J,LD).EQ.3) THEN
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
     &                      .OR. N.EQ.3 .OR. N.EQ.4) THEN
                  IF (FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD=FD
                ENDIF
                FDM=MAX(FDMW,FD)
                IF (FDMW.LT.FLOWER .AND. FA.LT.FLOWER)
     &                                      FDM=MAX(FDM,0.1D0)
                CFX=MAX((FDM-FAD)*AC-(FDM-FD)*V,0.0D0)
                FVX=FAD*AC+CFX
                FVM=FD*V
                FLFW(I,J,K)=FLFW(I,J,K)-DBLE(LSN)*MIN(FVX,FVM)
              ELSE
                LSW=LSN
                IF (SZ(I,J,LD).LT.0.0D0) LSW=-LSW
                DD0=AC
                IF (LSW.EQ.1) DD0=ZZ(2,LD)-AC
                ISW=ISTYP(I,J,LD)
                IF     (ISW.EQ.5) THEN
                  CALL VF_FSLP2F(SX(I,J,LD),SZ(I,J,LD),
     &                           XX(2,I)   ,DD0       ,
     &                           SA(I,J,LD),DFF        )
                ELSEIF (ISW.EQ.6) THEN
                  CALL VF_FSLP2F(SY(I,J,LD),SZ(I,J,LD),
     &                           YY(2,J)   ,DD0       ,
     &                           SA(I,J,LD),DFF        )
                ELSEIF (ISW.EQ.7) THEN
                  CALL VF_FSLP3F(SX(I,J,LD),SY(I,J,LD),SZ(I,J,LD),
     &                           XX(2,I)   ,YY(2,J)   ,DD0       ,
     &                           SA(I,J,LD),DFF                   )
                ELSE
                  CALL VF_A2ERR('VF_FCONVS','P.G ERROR(4).')
                ENDIF
                VG=ZZ(2,LD)*FF(I,J,LD)
                FVX=DFF*DD0
                IF (LSW.EQ.1) FVX=VG-FVX
                VG=VG*GGV(I,J,LD)
                FLFW(I,J,K)=FLFW(I,J,K)-DBLE(LSN)*MIN(FVX,VG)
              ENDIF
            ENDIF
 600      CONTINUE
 610    CONTINUE
 620  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
