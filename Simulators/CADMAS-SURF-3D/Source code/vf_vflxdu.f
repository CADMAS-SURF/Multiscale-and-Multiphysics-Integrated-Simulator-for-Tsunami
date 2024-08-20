      SUBROUTINE VF_VFLXDU(XX,YY,ZZ,UU,VV,WW,ANU,
     &                     GGX,GGY,GGZ,GLX,GLY,GLZ,BCU,
     &                     FLUU,FLUV,FLUW,
     &                     NF,INDX,INDY,INDZ,INDC,INDB)

CD=== 概要 ===========================================================

CDT   VF_VFLXDU:x方向流速の対流項および粘性項を計算する(DONORスキーム)

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
CD    BCU(NUMB)        : IN  : R*8 : x方向流速の境界値
CD    FLUU(@FOR-3D@)   : I/O : R*8 : x方向流速のx方向フラックス
CD    FLUV(@FOR-3D@)   : I/O : R*8 : x方向流速のy方向フラックス
CD    FLUW(@FOR-3D@)   : I/O : R*8 : x方向流速のz方向フラックス
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
      DIMENSION BCU(NUMB)
      DIMENSION FLUU(NUMI,NUMJ,NUMK),FLUV(NUMI,NUMJ,NUMK)
      DIMENSION FLUW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      LA=MYIS
      LB=MYIE
      IF (MYMIS.EQ.1) LA=LA+1

CD    -- 定数の計算 --
      SU=SCMVP
      SC=1.0D0-SCMVP

CD    -- (Gx*u)*uとGx*Nu*(2du/dx)の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=LA-1,LB
            IF (INDC(I,J,K).NE.-1) THEN

C             * (Gx*u)*u
              WIN=( GLX(I  ,J,K)*UU(I  ,J,K)
     &             +GLX(I+1,J,K)*UU(I+1,J,K))*0.5D0
              FG =WIN*(UU(I,J,K)+UU(I+1,J,K))*0.5D0
              FLUU(I,J,K)=FLUU(I,J,K)-SU*( MAX( WIN,0.0D0)*UU(I  ,J,K)
     &                                    -MAX(-WIN,0.0D0)*UU(I+1,J,K))
     &                               -SC*FG

C             * Gx*Nu*(2du/dx)
              FLUU(I,J,K)=FLUU(I,J,K)+(GGX(I,J,K)+GGX(I+1,J,K))
     &                      *ANU(I,J,K)*XX(4,I)*(UU(I+1,J,K)-UU(I,J,K))

            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- (Gy*v)*uとGy*Nu*(du/dy+dv/dx)の計算 --
      DO 220 K=2,NUMK
        DO 210 J=MYJS,MYJE+1
          DO 200 I=LA,LB
            IC=NF(I-1,J-1,K)*NF(I,J-1,K)*NF(I-1,J,K)*NF(I,J,K)
            IT=INDX(I,J  ,K)
            IB=INDX(I,J-1,K)

CD          -- 非計算格子 --
            IF (IC.NE.0 .OR. (IT.NE.0 .AND. IB.NE.0)) THEN

CD          -- 計算格子(通常処理) --
            ELSEIF (IT.EQ.0 .AND. IB.EQ.0) THEN

C             * (Gy*v)*u
CAKIY         ? 線形補間
              WIN=XX(6,I)*( XX(2,I-1)*GLY(I  ,J,K)*VV(I  ,J,K)
     &                     +XX(2,I  )*GLY(I-1,J,K)*VV(I-1,J,K))
              FG =WIN*YY(6,J)*(YY(2,J-1)*UU(I,J,K)+YY(2,J)*UU(I,J-1,K))
              FLUV(I,J,K)=FLUV(I,J,K)-SU*( MAX( WIN,0.0D0)*UU(I,J-1,K)
     &                                    -MAX(-WIN,0.0D0)*UU(I,J  ,K))
     &                               -SC*FG

C             * Gy*Nu*(du/dy+dv/dx)
C             ? 線形補間
              GG=XX(6,I)*(XX(2,I-1)*GGY(I,J,K)+XX(2,I)*GGY(I-1,J,K))
              ANR=YY(6,J)*( YY(2,J-1)*ANU(I  ,J  ,K)
     &                     +YY(2,J  )*ANU(I  ,J-1,K))
              ANL=YY(6,J)*( YY(2,J-1)*ANU(I-1,J  ,K)
     &                     +YY(2,J  )*ANU(I-1,J-1,K))
              AN=XX(6,I)*(XX(2,I-1)*ANR+XX(2,I)*ANL)
              D1=YY(5,J)*(UU(I,J,K)-UU(I  ,J-1,K))
              D2=XX(5,I)*(VV(I,J,K)-VV(I-1,J  ,K))
              FLUV(I,J,K)=FLUV(I,J,K)+GG*AN*(D1+D2)

CD          -- 計算格子(特殊処理) --
            ELSE

C             * 正側の流速値
              IS=INDY(I,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.3) THEN
                    BR=UU(I,J  ,K)
                  ELSE
                    BR=UU(I,J-1,K)
                  ENDIF
                ELSE
                  BR=BCU(IS)
                ENDIF
              ELSE
                BR=YY(6,J)*( YY(2,J-1)*(UU(I,J  ,K)+UU(I+1,J  ,K))
     &                      +YY(2,J  )*(UU(I,J-1,K)+UU(I+1,J-1,K)))
     &                    *0.5D0
              ENDIF

C             * 負側の流速値
              IS=INDY(I-1,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.3) THEN
                    BL=UU(I,J  ,K)
                  ELSE
                    BL=UU(I,J-1,K)
                  ENDIF
                ELSE
                  BL=BCU(IS)
                ENDIF
              ELSE
                BL=YY(6,J)*( YY(2,J-1)*(UU(I,J  ,K)+UU(I-1,J  ,K))
     &                      +YY(2,J  )*(UU(I,J-1,K)+UU(I-1,J-1,K)))
     &                    *0.5D0
              ENDIF

C             * (Gy*v)*u
C             ? 線形補間
              WIN=XX(6,I)*( XX(2,I-1)*GLY(I  ,J,K)*VV(I  ,J,K)
     &                     +XX(2,I  )*GLY(I-1,J,K)*VV(I-1,J,K))
              FG =XX(6,I)*( XX(2,I  )*GLY(I  ,J,K)*VV(I  ,J,K)*BR
     &                     +XX(2,I-1)*GLY(I-1,J,K)*VV(I-1,J,K)*BL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF     (IT.EQ.0 .AND. WIN.LT.0.0D0) THEN
!             IF     ((IT.EQ.0 .or. mgrank==1) .AND. WIN.LT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLUV(I,J,K)=FLUV(I,J,K)-SU*WIN*UU(I,J  ,K)-SC*FG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ELSEIF (IB.EQ.0 .AND. WIN.GT.0.0D0) THEN
!             ELSEIF ((IB.EQ.0 .or. mgrank==1) .AND. WIN.GT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLUV(I,J,K)=FLUV(I,J,K)-SU*WIN*UU(I,J-1,K)-SC*FG
              ELSE
                FLUV(I,J,K)=FLUV(I,J,K)-FG
              ENDIF

C             * Gy*Nu*(du/dy+dv/dx)
C             ? 線形補間
              GG=XX(6,I)*(XX(2,I-1)*GGY(I,J,K)+XX(2,I)*GGY(I-1,J,K))
              FG=XX(6,I)*(XX(2,I)*BR+XX(2,I-1)*BL)
              IF     (NF(I,J  ,K).EQ.-1) THEN
                ANR=ANU(I,J-1,K)
              ELSEIF (NF(I,J-1,K).EQ.-1) THEN
                ANR=ANU(I,J  ,K)
              ELSE
                ANR=YY(6,J)*( YY(2,J-1)*ANU(I,J  ,K)
     &                       +YY(2,J  )*ANU(I,J-1,K))
              ENDIF
              IF     (NF(I-1,J  ,K).EQ.-1) THEN
                ANL=ANU(I-1,J-1,K)
              ELSEIF (NF(I-1,J-1,K).EQ.-1) THEN
                ANL=ANU(I-1,J  ,K)
              ELSE
                ANL=YY(6,J)*( YY(2,J-1)*ANU(I-1,J  ,K)
     &                       +YY(2,J  )*ANU(I-1,J-1,K))
              ENDIF
              AN=XX(6,I)*(XX(2,I-1)*ANR+XX(2,I)*ANL)
              IF (IT.EQ.0) THEN
                D1=YY(4,J  )*(UU(I,J  ,K)-FG)*2.0D0
              ELSE
                D1=YY(4,J-1)*(FG-UU(I,J-1,K))*2.0D0
              ENDIF
              D2=XX(5,I)*(VV(I,J,K)-VV(I-1,J,K))
              FLUV(I,J,K)=FLUV(I,J,K)+GG*AN*(D1+D2)

            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- (Gz*w)*uとGz*Nu*(du/dz+dw/dx)の計算 --
      DO 320 K=2,NUMK
        DO 310 J=MYJS,MYJE
          DO 300 I=LA,LB
            IC=NF(I-1,J,K-1)*NF(I,J,K-1)*NF(I-1,J,K)*NF(I,J,K)
            IT=INDX(I,J,K  )
            IB=INDX(I,J,K-1)

CD          -- 非計算格子 --
            IF (IC.NE.0 .OR. (IT.NE.0 .AND. IB.NE.0)) THEN

CD          -- 計算格子(通常処理) --
            ELSEIF (IT.EQ.0 .AND. IB.EQ.0) THEN

C             * (Gz*w)*u
C             ? 線形補間
              WIN=XX(6,I)*( XX(2,I-1)*GLZ(I  ,J,K)*WW(I  ,J,K)
     &                     +XX(2,I  )*GLZ(I-1,J,K)*WW(I-1,J,K))
              FG =WIN*ZZ(6,K)*(ZZ(2,K-1)*UU(I,J,K)+ZZ(2,K)*UU(I,J,K-1))
              FLUW(I,J,K)=FLUW(I,J,K)-SU*( MAX( WIN,0.0D0)*UU(I,J,K-1)
     &                                    -MAX(-WIN,0.0D0)*UU(I,J,K  ))
     &                               -SC*FG

C             * Gz*Nu*(du/dz+dw/dx)
C             ? 線形補間
              GG=XX(6,I)*(XX(2,I-1)*GGZ(I,J,K)+XX(2,I)*GGZ(I-1,J,K))
              ANR=ZZ(6,K)*( ZZ(2,K-1)*ANU(I  ,J,K  )
     &                     +ZZ(2,K  )*ANU(I  ,J,K-1))
              ANL=ZZ(6,K)*( ZZ(2,K-1)*ANU(I-1,J,K  )
     &                     +ZZ(2,K  )*ANU(I-1,J,K-1))
              AN=XX(6,I)*(XX(2,I-1)*ANR+XX(2,I)*ANL)
              D1=ZZ(5,K)*(UU(I,J,K)-UU(I  ,J,K-1))
              D2=XX(5,I)*(WW(I,J,K)-WW(I-1,J,K  ))
              FLUW(I,J,K)=FLUW(I,J,K)+GG*AN*(D1+D2)

CD          -- 計算格子(特殊処理) --
            ELSE

C             * 正側の流速値
              IS=INDZ(I,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.5) THEN
                    BR=UU(I,J,K  )
                  ELSE
                    BR=UU(I,J,K-1)
                  ENDIF
                ELSE
                  BR=BCU(IS)
                ENDIF
              ELSE
                BR=ZZ(6,K)*( ZZ(2,K-1)*(UU(I,J,K  )+UU(I+1,J,K  ))
     &                      +ZZ(2,K  )*(UU(I,J,K-1)+UU(I+1,J,K-1)))
     &                    *0.5D0
              ENDIF

C             * 負側の流速値
              IS=INDZ(I-1,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.5) THEN
                    BL=UU(I,J,K  )
                  ELSE
                    BL=UU(I,J,K-1)
                  ENDIF
                ELSE
                  BL=BCU(IS)
                ENDIF
              ELSE
                BL=ZZ(6,K)*( ZZ(2,K-1)*(UU(I,J,K  )+UU(I-1,J,K  ))
     &                      +ZZ(2,K  )*(UU(I,J,K-1)+UU(I-1,J,K-1)))
     &                    *0.5D0
              ENDIF

C             * (Gz*w)*u
C             ? 線形補間
              WIN=XX(6,I)*( XX(2,I-1)*GLZ(I  ,J,K)*WW(I  ,J,K)
     &                     +XX(2,I  )*GLZ(I-1,J,K)*WW(I-1,J,K))
              FG =XX(6,I)*( XX(2,I  )*GLZ(I  ,J,K)*WW(I  ,J,K)*BR
     &                     +XX(2,I-1)*GLZ(I-1,J,K)*WW(I-1,J,K)*BL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF     (IT.EQ.0 .AND. WIN.LT.0.0D0) THEN
!             IF     ((IT.EQ.0 .or. mgrank==1) .AND. WIN.LT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLUW(I,J,K)=FLUW(I,J,K)-SU*WIN*UU(I,J,K  )-SC*FG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ELSEIF (IB.EQ.0 .AND. WIN.GT.0.0D0) THEN
!             ELSEIF ((IB.EQ.0 .or. mgrank==1) .AND. WIN.GT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLUW(I,J,K)=FLUW(I,J,K)-SU*WIN*UU(I,J,K-1)-SC*FG
              ELSE
                FLUW(I,J,K)=FLUW(I,J,K)-FG
              ENDIF

C             * Gz*Nu*(du/dz+dw/dx)
C             ? 線形補間
              GG=XX(6,I)*(XX(2,I-1)*GGZ(I,J,K)+XX(2,I)*GGZ(I-1,J,K))
              FG=XX(6,I)*(XX(2,I)*BR+XX(2,I-1)*BL)
              IF     (NF(I,J,K  ).EQ.-1) THEN
                ANR=ANU(I,J,K-1)
              ELSEIF (NF(I,J,K-1).EQ.-1) THEN
                ANR=ANU(I,J,K  )
              ELSE
                ANR=ZZ(6,K)*( ZZ(2,K-1)*ANU(I,J,K  )
     &                       +ZZ(2,K  )*ANU(I,J,K-1))
              ENDIF
              IF     (NF(I-1,J,K  ).EQ.-1) THEN
                ANL=ANU(I-1,J,K-1)
              ELSEIF (NF(I-1,J,K-1).EQ.-1) THEN
                ANL=ANU(I-1,J,K  )
              ELSE
                ANL=ZZ(6,K)*( ZZ(2,K-1)*ANU(I-1,J,K  )
     &                       +ZZ(2,K  )*ANU(I-1,J,K-1))
              ENDIF
              AN=XX(6,I)*(XX(2,I-1)*ANR+XX(2,I)*ANL)
              IF (IT.EQ.0) THEN
                D1=ZZ(4,K  )*(UU(I,J,K  )-FG)*2.0D0
              ELSE
                D1=ZZ(4,K-1)*(FG-UU(I,J,K-1))*2.0D0
              ENDIF
              D2=XX(5,I)*(WW(I,J,K)-WW(I-1,J,K))
              FLUW(I,J,K)=FLUW(I,J,K)+GG*AN*(D1+D2)

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
