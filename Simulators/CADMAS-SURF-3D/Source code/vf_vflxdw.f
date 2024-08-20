      SUBROUTINE VF_VFLXDW(XX,YY,ZZ,UU,VV,WW,ANU,
     &                     GGX,GGY,GGZ,GLX,GLY,GLZ,BCW,
     &                     FLWU,FLWV,FLWW,
     &                     NF,INDX,INDY,INDZ,INDC,INDB)

CD=== 概要 ===========================================================

CDT   VF_VFLXDW:z方向流速の対流項によるフラックスの計算(DONORスキーム)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    ANU(@FOR-3D@)    : IN  : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    GLX(@FOR-3D@)    : IN  : R*8 : =GGX+(1-GGX)*CM
CD    GLY(@FOR-3D@)    : IN  : R*8 : =GGY+(1-GGY)*CM
CD    GLZ(@FOR-3D@)    : IN  : R*8 : =GGZ+(1-GGZ)*CM
CD    BCW(NUMB)        : IN  : R*8 : z方向流速の境界値
CD    FLWU(@FOR-3D@)   : I/O : R*8 : z方向流速のx方向フラックス
CD    FLWV(@FOR-3D@)   : I/O : R*8 : z方向流速のy方向フラックス
CD    FLWW(@FOR-3D@)   : I/O : R*8 : z方向流速のz方向フラックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)   : IN  : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),ANU (NUMI,NUMJ,NUMK)
      DIMENSION GGX (NUMI,NUMJ,NUMK),GGY (NUMI,NUMJ,NUMK)
      DIMENSION GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GLX (NUMI,NUMJ,NUMK),GLY (NUMI,NUMJ,NUMK)
      DIMENSION GLZ (NUMI,NUMJ,NUMK)
      DIMENSION BCW(NUMB)
      DIMENSION FLWU(NUMI,NUMJ,NUMK),FLWV(NUMI,NUMJ,NUMK)
      DIMENSION FLWW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 定数の計算 --
      SU=SCMVP
      SC=1.0D0-SCMVP

CD    -- (Gz*w)*wとGz*Nu*(2dw/dz)の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN

C             * (Gz*w)*w
              WIN=( GLZ(I,J,K  )*WW(I,J,K  )
     &             +GLZ(I,J,K+1)*WW(I,J,K+1))*0.5D0
              FG =WIN*(WW(I,J,K)+WW(I,J,K+1))*0.5D0
              FLWW(I,J,K)=FLWW(I,J,K)-SU*( MAX( WIN,0.0D0)*WW(I,J,K  )
     &                                    -MAX(-WIN,0.0D0)*WW(I,J,K+1))
     &                               -SC*FG

C             * Gz*Nu*(2dw/dz)
              FLWW(I,J,K)=FLWW(I,J,K)+(GGZ(I,J,K)+GGZ(I,J,K+1))
     &                      *ANU(I,J,K)*ZZ(4,K)*(WW(I,J,K+1)-WW(I,J,K))

            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- (Gx*u)*wとGx*Nu*(dw/dx+du/dz)の計算 --
      DO 220 K=3,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE+1
            IC=NF(I-1,J,K-1)*NF(I-1,J,K)*NF(I,J,K-1)*NF(I,J,K)
            IT=INDZ(I  ,J,K)
            IB=INDZ(I-1,J,K)

CD          -- 非計算格子 --
            IF (IC.NE.0 .OR. (IT.NE.0 .AND. IB.NE.0)) THEN

CD          -- 計算格子(通常処理) --
            ELSEIF (IT.EQ.0 .AND. IB.EQ.0) THEN

C             * (Gx*u)*w
C             ? 線形補間
              WIN=ZZ(6,K)*( ZZ(2,K-1)*GLX(I,J,K  )*UU(I,J,K  )
     &                     +ZZ(2,K  )*GLX(I,J,K-1)*UU(I,J,K-1))
              FG =WIN*XX(6,I)*(XX(2,I-1)*WW(I,J,K)+XX(2,I)*WW(I-1,J,K))
              FLWU(I,J,K)=FLWU(I,J,K)-SU*( MAX( WIN,0.0D0)*WW(I-1,J,K)
     &                                    -MAX(-WIN,0.0D0)*WW(I  ,J,K))
     &                               -SC*FG

C             * Gx*Nu*(dw/dx+du/dz)
C             ? 線形補間
              GG=ZZ(6,K)*(ZZ(2,K-1)*GGX(I,J,K)+ZZ(2,K)*GGX(I,J,K-1))
              ANR=XX(6,I)*( XX(2,I-1)*ANU(I  ,J,K  )
     &                     +XX(2,I  )*ANU(I-1,J,K  ))
              ANL=XX(6,I)*( XX(2,I-1)*ANU(I  ,J,K-1)
     &                     +XX(2,I  )*ANU(I-1,J,K-1))
              AN=ZZ(6,K)*(ZZ(2,K-1)*ANR+ZZ(2,K)*ANL)
              D1=XX(5,I)*(WW(I,J,K)-WW(I-1,J,K  ))
              D2=ZZ(5,K)*(UU(I,J,K)-UU(I  ,J,K-1))
              FLWU(I,J,K)=FLWU(I,J,K)+GG*AN*(D1+D2)

CD          -- 計算格子(特殊処理) --
            ELSE

C             * 正側の流速値
              IS=INDX(I,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.1) THEN
                    BR=WW(I  ,J,K)
                  ELSE
                    BR=WW(I-1,J,K)
                  ENDIF
                ELSE
                  BR=BCW(IS)
                ENDIF
              ELSE
                BR=XX(6,I)*( XX(2,I-1)*(WW(I  ,J,K)+WW(I  ,J,K+1))
     &                      +XX(2,I  )*(WW(I-1,J,K)+WW(I-1,J,K+1)))
     &                    *0.5D0
              ENDIF

C             * 負側の流速値
              IS=INDX(I,J,K-1)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.1) THEN
                    BL=WW(I  ,J,K)
                  ELSE
                    BL=WW(I-1,J,K)
                  ENDIF
                ELSE
                  BL=BCW(IS)
                ENDIF
              ELSE
                BL=XX(6,I)*( XX(2,I-1)*(WW(I  ,J,K)+WW(I  ,J,K-1))
     &                      +XX(2,I  )*(WW(I-1,J,K)+WW(I-1,J,K-1)))
     &                    *0.5D0
              ENDIF

C             * (Gx*u)*w
C             ? 線形補間
              WIN=ZZ(6,K)*( ZZ(2,K-1)*GLX(I,J,K  )*UU(I,J,K  )
     &                     +ZZ(2,K  )*GLX(I,J,K-1)*UU(I,J,K-1))
              FG =ZZ(6,K)*( ZZ(2,K  )*GLX(I,J,K  )*UU(I,J,K  )*BR
     &                     +ZZ(2,K-1)*GLX(I,J,K-1)*UU(I,J,K-1)*BL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF     (IT.EQ.0 .AND. WIN.LT.0.0D0) THEN
!             IF     ((IT.EQ.0 .or. mgrank==1) .AND. WIN.LT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLWU(I,J,K)=FLWU(I,J,K)-SU*WIN*WW(I  ,J,K)-SC*FG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ELSEIF (IB.EQ.0 .AND. WIN.GT.0.0D0) THEN
!             ELSEIF ((IB.EQ.0 .or. mgrank==1) .AND. WIN.GT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLWU(I,J,K)=FLWU(I,J,K)-SU*WIN*WW(I-1,J,K)-SC*FG
              ELSE
                FLWU(I,J,K)=FLWU(I,J,K)-FG
              ENDIF

C             * Gx*Nu*(dw/dx+du/dz)
C             ? 線形補間
              GG=ZZ(6,K)*(ZZ(2,K-1)*GGX(I,J,K)+ZZ(2,K)*GGX(I,J,K-1))
              FG=ZZ(6,K)*(ZZ(2,K)*BR+ZZ(2,K-1)*BL)
              IF     (NF(I  ,J,K).EQ.-1) THEN
                ANR=ANU(I-1,J,K)
              ELSEIF (NF(I-1,J,K).EQ.-1) THEN
                ANR=ANU(I  ,J,K)
              ELSE
                ANR=XX(6,I)*( XX(2,I-1)*ANU(I  ,J,K)
     &                       +XX(2,I  )*ANU(I-1,J,K))
              ENDIF
              IF     (NF(I  ,J,K-1).EQ.-1) THEN
                ANL=ANU(I-1,J,K-1)
              ELSEIF (NF(I-1,J,K-1).EQ.-1) THEN
                ANL=ANU(I  ,J,K-1)
              ELSE
                ANL=XX(6,I)*( XX(2,I-1)*ANU(I  ,J,K-1)
     &                       +XX(2,I  )*ANU(I-1,J,K-1))
              ENDIF
              AN=ZZ(6,K)*(ZZ(2,K-1)*ANR+ZZ(2,K)*ANL)
              IF (IT.EQ.0) THEN
                D1=XX(4,I  )*(WW(I  ,J,K)-FG)*2.0D0
              ELSE
                D1=XX(4,I-1)*(FG-WW(I-1,J,K))*2.0D0
              ENDIF
              D2=ZZ(5,K)*(UU(I,J,K)-UU(I,J,K-1))
              FLWU(I,J,K)=FLWU(I,J,K)+GG*AN*(D1+D2)

            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- (Gy*v)*wとGy*Nu*(dw/dy+dv/dz)の計算 --
      DO 320 K=3,NUMK-1
        DO 310 J=MYJS,MYJE+1
          DO 300 I=MYIS,MYIE
            IC=NF(I,J-1,K-1)*NF(I,J-1,K)*NF(I,J,K-1)*NF(I,J,K)
            IT=INDZ(I,J  ,K)
            IB=INDZ(I,J-1,K)

CD          -- 非計算格子 --
            IF (IC.NE.0 .OR. (IT.NE.0 .AND. IB.NE.0)) THEN

CD          -- 計算格子(通常処理) --
            ELSEIF (IT.EQ.0 .AND. IB.EQ.0) THEN

C             * (Gy*v)*w
C             ? 線形補間
              WIN=ZZ(6,K)*( ZZ(2,K-1)*GLY(I,J,K  )*VV(I,J,K  )
     &                     +ZZ(2,K  )*GLY(I,J,K-1)*VV(I,J,K-1))
              FG =WIN*YY(6,J)*(YY(2,J-1)*WW(I,J,K)+YY(2,J)*WW(I,J-1,K))
              FLWV(I,J,K)=FLWV(I,J,K)-SU*( MAX( WIN,0.0D0)*WW(I,J-1,K)
     &                                    -MAX(-WIN,0.0D0)*WW(I,J  ,K))
     &                               -SC*FG

C             * Gy*Nu*(dw/dy+dv/dz)
C             ? 線形補間
              GG=ZZ(6,K)*(ZZ(2,K-1)*GGY(I,J,K)+ZZ(2,K)*GGY(I,J,K-1))
              ANR=YY(6,J)*( YY(2,J-1)*ANU(I,J  ,K  )
     &                     +YY(2,J  )*ANU(I,J-1,K  ))
              ANL=YY(6,J)*( YY(2,J-1)*ANU(I,J  ,K-1)
     &                     +YY(2,J  )*ANU(I,J-1,K-1))
              AN=ZZ(6,K)*(ZZ(2,K-1)*ANR+ZZ(2,K)*ANL)
              D1=YY(5,J)*(WW(I,J,K)-WW(I,J-1,K  ))
              D2=ZZ(5,K)*(VV(I,J,K)-VV(I,J  ,K-1))
              FLWV(I,J,K)=FLWV(I,J,K)+GG*AN*(D1+D2)

CD          -- 計算格子(特殊処理) --
            ELSE

C             * 正側の流速値
              IS=INDY(I,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.3) THEN
                    BR=WW(I,J  ,K)
                  ELSE
                    BR=WW(I,J-1,K)
                  ENDIF
                ELSE
                  BR=BCW(IS)
                ENDIF
              ELSE
                BR=YY(6,J)*( YY(2,J-1)*(WW(I,J  ,K)+WW(I,J  ,K+1))
     &                      +YY(2,J  )*(WW(I,J-1,K)+WW(I,J-1,K+1)))
     &                    *0.5D0
              ENDIF

C             * 負側の流速値
              IS=INDY(I,J,K-1)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.3) THEN
                    BL=WW(I,J  ,K)
                  ELSE
                    BL=WW(I,J-1,K)
                  ENDIF
                ELSE
                  BL=BCW(IS)
                ENDIF
              ELSE
                BL=YY(6,J)*( YY(2,J-1)*(WW(I,J  ,K)+WW(I,J  ,K-1))
     &                      +YY(2,J  )*(WW(I,J-1,K)+WW(I,J-1,K-1)))
     &                    *0.5D0
              ENDIF

C             * (Gy*v)*w
C             ? 線形補間
              WIN=ZZ(6,K)*( ZZ(2,K-1)*GLY(I,J,K  )*VV(I,J,K  )
     &                     +ZZ(2,K  )*GLY(I,J,K-1)*VV(I,J,K-1))
              FG =ZZ(6,K)*( ZZ(2,K  )*GLY(I,J,K  )*VV(I,J,K  )*BR
     &                     +ZZ(2,K-1)*GLY(I,J,K-1)*VV(I,J,K-1)*BL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF     (IT.EQ.0 .AND. WIN.LT.0.0D0) THEN
!             IF     ((IT.EQ.0 .or. mgrank==1) .AND. WIN.LT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLWV(I,J,K)=FLWV(I,J,K)-SU*WIN*WW(I,J  ,K)-SC*FG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ELSEIF (IB.EQ.0 .AND. WIN.GT.0.0D0) THEN
!             ELSEIF ((IB.EQ.0 .or. mgrank==1) .AND. WIN.GT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLWV(I,J,K)=FLWV(I,J,K)-SU*WIN*WW(I,J-1,K)-SC*FG
              ELSE
                FLWV(I,J,K)=FLWV(I,J,K)-FG
              ENDIF

C             * Gy*Nu*(dw/dy+dv/dz)
C             ? 線形補間
              GG=ZZ(6,K)*(ZZ(2,K-1)*GGY(I,J,K)+ZZ(2,K)*GGY(I,J,K-1))
              FG=ZZ(6,K)*(ZZ(2,K)*BR+ZZ(2,K-1)*BL)
              IF     (NF(I,J  ,K).EQ.-1) THEN
                ANR=ANU(I,J-1,K)
              ELSEIF (NF(I,J-1,K).EQ.-1) THEN
                ANR=ANU(I,J  ,K)
              ELSE
                ANR=YY(6,J)*( YY(2,J-1)*ANU(I,J  ,K)
     &                       +YY(2,J  )*ANU(I,J-1,K))
              ENDIF
              IF     (NF(I,J  ,K-1).EQ.-1) THEN
                ANL=ANU(I,J-1,K-1)
              ELSEIF (NF(I,J-1,K-1).EQ.-1) THEN
                ANL=ANU(I,J  ,K-1)
              ELSE
                ANL=YY(6,J)*( YY(2,J-1)*ANU(I,J  ,K-1)
     &                       +YY(2,J  )*ANU(I,J-1,K-1))
              ENDIF
              AN=ZZ(6,K)*(ZZ(2,K-1)*ANR+ZZ(2,K)*ANL)
              IF (IT.EQ.0) THEN
                D1=YY(4,J  )*(WW(I,J  ,K)-FG)*2.0D0
              ELSE
                D1=YY(4,J-1)*(FG-WW(I,J-1,K))*2.0D0
              ENDIF
              D2=ZZ(5,K)*(VV(I,J,K)-VV(I,J,K-1))
              FLWV(I,J,K)=FLWV(I,J,K)+GG*AN*(D1+D2)

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
