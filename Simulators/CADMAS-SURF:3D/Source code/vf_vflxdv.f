      SUBROUTINE VF_VFLXDV(XX,YY,ZZ,UU,VV,WW,ANU,
     &                     GGX,GGY,GGZ,GLX,GLY,GLZ,BCV,
     &                     FLVU,FLVV,FLVW,
     &                     NF,INDX,INDY,INDZ,INDC,INDB)

CD=== 概要 ===========================================================

CDT   VF_VFLXDV:y方向流速の対流項および粘性項を計算する(DONORスキーム)

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
CD    GGZ(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GLX(@FOR-3D@)    : IN  : R*8 : =GGX+(1-GGX)*CM
CD    GLY(@FOR-3D@)    : IN  : R*8 : =GGY+(1-GGY)*CM
CD    GLZ(@FOR-3D@)    : IN  : R*8 : =GGZ+(1-GGZ)*CM
CD    BCV(NUMB)        : IN  : R*8 : y方向流速の境界値
CD    FLVU(@FOR-3D@)   : I/O : R*8 : y方向流速のx方向フラックス
CD    FLVV(@FOR-3D@)   : I/O : R*8 : y方向流速のy方向フラックス
CD    FLVW(@FOR-3D@)   : I/O : R*8 : y方向流速のz方向フラックス
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
      DIMENSION BCV(NUMB)
      DIMENSION FLVU(NUMI,NUMJ,NUMK),FLVV(NUMI,NUMJ,NUMK)
      DIMENSION FLVW(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      LA=MYJS
      LB=MYJE
      IF (MYMJS.EQ.1) LA=LA+1

CD    -- 定数の計算 --
      SU=SCMVP
      SC=1.0D0-SCMVP

CD    -- (Gy*v)*vとGy*Nu*(2dv/dy)の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=LA-1,LB
          DO 100 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN

C             * (Gy*v)*v
              WIN=( GLY(I,J  ,K)*VV(I,J  ,K)
     &             +GLY(I,J+1,K)*VV(I,J+1,K))*0.5D0
              FG =WIN*(VV(I,J,K)+VV(I,J+1,K))*0.5D0
              FLVV(I,J,K)=FLVV(I,J,K)-SU*( MAX( WIN,0.0D0)*VV(I,J  ,K)
     &                                    -MAX(-WIN,0.0D0)*VV(I,J+1,K))
     &                               -SC*FG

C             * Gy*Nu*(2dv/dy)
              FLVV(I,J,K)=FLVV(I,J,K)+(GGY(I,J,K)+GGY(I,J+1,K))
     &                      *ANU(I,J,K)*YY(4,J)*(VV(I,J+1,K)-VV(I,J,K))

            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- (Gx*u)*vとGx*Nu*(dv/dx+du/dy)の計算 --
      DO 220 K=2,NUMK
        DO 210 J=LA,LB
          DO 200 I=MYIS,MYIE+1
            IC=NF(I-1,J-1,K)*NF(I-1,J,K)*NF(I,J-1,K)*NF(I,J,K)
            IT=INDY(I  ,J,K)
            IB=INDY(I-1,J,K)

CD          -- 非計算格子 --
            IF (IC.NE.0 .OR. (IT.NE.0 .AND. IB.NE.0)) THEN

CD          -- 計算格子(通常処理) --
            ELSEIF (IT.EQ.0 .AND. IB.EQ.0) THEN

C             * (Gx*u)*v
C             ? 線形補間
              WIN=YY(6,J)*( YY(2,J-1)*GLX(I,J  ,K)*UU(I,J  ,K)
     &                     +YY(2,J  )*GLX(I,J-1,K)*UU(I,J-1,K))
              FG =WIN*XX(6,I)*(XX(2,I-1)*VV(I,J,K)+XX(2,I)*VV(I-1,J,K))
              FLVU(I,J,K)=FLVU(I,J,K)-SU*( MAX( WIN,0.0D0)*VV(I-1,J,K)
     &                                    -MAX(-WIN,0.0D0)*VV(I  ,J,K))
     &                               -SC*FG

C             * Gx*Nu*(dv/dx+du/dy)
C             ? 線形補間
              GG=YY(6,J)*(YY(2,J-1)*GGX(I,J,K)+YY(2,J)*GGX(I,J-1,K))
              ANR=XX(6,I)*( XX(2,I-1)*ANU(I  ,J  ,K)
     &                     +XX(2,I  )*ANU(I-1,J  ,K))
              ANL=XX(6,I)*( XX(2,I-1)*ANU(I  ,J-1,K)
     &                     +XX(2,I  )*ANU(I-1,J-1,K))
              AN=YY(6,J)*(YY(2,J-1)*ANR+YY(2,J)*ANL)
              D1=XX(5,I)*(VV(I,J,K)-VV(I-1,J  ,K))
              D2=YY(5,J)*(UU(I,J,K)-UU(I  ,J-1,K))
              FLVU(I,J,K)=FLVU(I,J,K)+GG*AN*(D1+D2)

CD          -- 計算格子(特殊処理) --
            ELSE

C             * 正側の流速値
              IS=INDX(I,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.1) THEN
                    BR=VV(I  ,J,K)
                  ELSE
                    BR=VV(I-1,J,K)
                  ENDIF
                ELSE
                  BR=BCV(IS)
                ENDIF
              ELSE
                BR=XX(6,I)*( XX(2,I-1)*(VV(I  ,J,K)+VV(I  ,J+1,K))
     &                      +XX(2,I  )*(VV(I-1,J,K)+VV(I-1,J+1,K)))
     &                    *0.5D0
              ENDIF

C             * 負側の流速値
              IS=INDX(I,J-1,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.1) THEN
                    BL=VV(I  ,J,K)
                  ELSE
                    BL=VV(I-1,J,K)
                  ENDIF
                ELSE
                  BL=BCV(IS)
                ENDIF
              ELSE
                BL=XX(6,I)*( XX(2,I-1)*(VV(I  ,J,K)+VV(I  ,J-1,K))
     &                      +XX(2,I  )*(VV(I-1,J,K)+VV(I-1,J-1,K)))
     &                    *0.5D0
              ENDIF

C             * (Gx*u)*v
C             ? 線形補間
              WIN=YY(6,J)*( YY(2,J-1)*GLX(I,J  ,K)*UU(I,J  ,K)
     &                     +YY(2,J  )*GLX(I,J-1,K)*UU(I,J-1,K))
              FG =YY(6,J)*( YY(2,J  )*GLX(I,J  ,K)*UU(I,J  ,K)*BR
     &                     +YY(2,J-1)*GLX(I,J-1,K)*UU(I,J-1,K)*BL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF     (IT.EQ.0 .AND. WIN.LT.0.0D0) THEN
!             IF     ((IT.EQ.0 .or. mgrank==1) .AND. WIN.LT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLVU(I,J,K)=FLVU(I,J,K)-SU*WIN*VV(I  ,J,K)-SC*FG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ELSEIF (IB.EQ.0 .AND. WIN.GT.0.0D0) THEN
!             ELSEIF ((IB.EQ.0 .or. mgrank==1) .AND. WIN.GT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLVU(I,J,K)=FLVU(I,J,K)-SU*WIN*VV(I-1,J,K)-SC*FG
              ELSE
                FLVU(I,J,K)=FLVU(I,J,K)-FG
              ENDIF

C             * Gx*Nu*(dv/dx+du/dy)
C             ? 線形補間
              GG=YY(6,J)*(YY(2,J-1)*GGX(I,J,K)+YY(2,J)*GGX(I,J-1,K))
              FG=YY(6,J)*(YY(2,J)*BR+YY(2,J-1)*BL)
              IF     (NF(I  ,J,K).EQ.-1) THEN
                ANR=ANU(I-1,J,K)
              ELSEIF (NF(I-1,J,K).EQ.-1) THEN
                ANR=ANU(I  ,J,K)
              ELSE
                ANR=XX(6,I)*( XX(2,I-1)*ANU(I  ,J,K)
     &                       +XX(2,I  )*ANU(I-1,J,K))
              ENDIF
              IF     (NF(I  ,J-1,K).EQ.-1) THEN
                ANL=ANU(I-1,J-1,K)
              ELSEIF (NF(I-1,J-1,K).EQ.-1) THEN
                ANL=ANU(I  ,J-1,K)
              ELSE
                ANL=XX(6,I)*( XX(2,I-1)*ANU(I  ,J-1,K)
     &                       +XX(2,I  )*ANU(I-1,J-1,K))
              ENDIF
              AN=YY(6,J)*(YY(2,J-1)*ANR+YY(2,J)*ANL)
              IF (IT.EQ.0) THEN
                D1=XX(4,I  )*(VV(I  ,J,K)-FG)*2.0D0
              ELSE
                D1=XX(4,I-1)*(FG-VV(I-1,J,K))*2.0D0
              ENDIF
              D2=YY(5,J)*(UU(I,J,K)-UU(I,J-1,K))
              FLVU(I,J,K)=FLVU(I,J,K)+GG*AN*(D1+D2)

            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- (Gz*w)*vとGz*Nu*(dv/dz+dw/dy)の計算 --
      DO 320 K=2,NUMK
        DO 310 J=LA,LB
          DO 300 I=MYIS,MYIE
            IC=NF(I,J-1,K-1)*NF(I,J,K-1)*NF(I,J-1,K)*NF(I,J,K)
            IT=INDY(I,J,K  )
            IB=INDY(I,J,K-1)

CD          -- 非計算格子 --
            IF (IC.NE.0 .OR. (IT.NE.0 .AND. IB.NE.0)) THEN

CD          -- 計算格子(通常処理) --
            ELSEIF (IT.EQ.0 .AND. IB.EQ.0) THEN

C             * (Gz*w)*v
C             ? 線形補間
              WIN=YY(6,J)*( YY(2,J-1)*GLZ(I,J  ,K)*WW(I,J  ,K)
     &                     +YY(2,J  )*GLZ(I,J-1,K)*WW(I,J-1,K))
              FG =WIN*ZZ(6,K)*(ZZ(2,K-1)*VV(I,J,K)+ZZ(2,K)*VV(I,J,K-1))
              FLVW(I,J,K)=FLVW(I,J,K)-SU*( MAX( WIN,0.0D0)*VV(I,J,K-1)
     &                                    -MAX(-WIN,0.0D0)*VV(I,J,K  ))
     &                               -SC*FG

C             * Gz*Nu*(dv/dz+dw/dy)
C             ? 線形補間
              GG=YY(6,J)*(YY(2,J-1)*GGZ(I,J,K)+YY(2,J)*GGZ(I,J-1,K))
              ANR=ZZ(6,K)*( ZZ(2,K-1)*ANU(I,J  ,K  )
     &                     +ZZ(2,K  )*ANU(I,J  ,K-1))
              ANL=ZZ(6,K)*( ZZ(2,K-1)*ANU(I,J-1,K  )
     &                     +ZZ(2,K  )*ANU(I,J-1,K-1))
              AN=YY(6,J)*(YY(2,J-1)*ANR+YY(2,J)*ANL)
              D1=ZZ(5,K)*(VV(I,J,K)-VV(I,J  ,K-1))
              D2=YY(5,J)*(WW(I,J,K)-WW(I,J-1,K  ))
              FLVW(I,J,K)=FLVW(I,J,K)+GG*AN*(D1+D2)

CD          -- 計算格子(特殊処理) --
            ELSE

C             * 正側の流速値
              IS=INDZ(I,J,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.5) THEN
                    BR=VV(I,J,K  )
                  ELSE
                    BR=VV(I,J,K-1)
                  ENDIF
                ELSE
                  BR=BCV(IS)
                ENDIF
              ELSE
                BR=ZZ(6,K)*( ZZ(2,K-1)*(VV(I,J,K  )+VV(I,J+1,K  ))
     &                      +ZZ(2,K  )*(VV(I,J,K-1)+VV(I,J+1,K-1)))
     &                    *0.5D0
              ENDIF

C             * 負側の流速値
              IS=INDZ(I,J-1,K)
              IF (IS.GE.1) THEN
                IF (INDB(3,IS).EQ.1 .OR. INDB(3,IS).EQ.4) THEN
                  IF (INDB(2,IS).EQ.5) THEN
                    BL=VV(I,J,K  )
                  ELSE
                    BL=VV(I,J,K-1)
                  ENDIF
                ELSE
                  BL=BCV(IS)
                ENDIF
              ELSE
                BL=ZZ(6,K)*( ZZ(2,K-1)*(VV(I,J,K  )+VV(I,J-1,K  ))
     &                      +ZZ(2,K  )*(VV(I,J,K-1)+VV(I,J-1,K-1)))
     &                    *0.5D0
              ENDIF

C             * (Gz*w)*v
C             ? 線形補間
              WIN=YY(6,J)*( YY(2,J-1)*GLZ(I,J  ,K)*WW(I,J  ,K)
     &                     +YY(2,J  )*GLZ(I,J-1,K)*WW(I,J-1,K))
              FG =YY(6,J)*( YY(2,J  )*GLZ(I,J  ,K)*WW(I,J  ,K)*BR
     &                     +YY(2,J-1)*GLZ(I,J-1,K)*WW(I,J-1,K)*BL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              IF     (IT.EQ.0 .AND. WIN.LT.0.0D0) THEN
!             IF     ((IT.EQ.0 .or. mgrank==1) .AND. WIN.LT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLVW(I,J,K)=FLVW(I,J,K)-SU*WIN*VV(I,J,K  )-SC*FG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ELSEIF (IB.EQ.0 .AND. WIN.GT.0.0D0) THEN
!             ELSEIF ((IB.EQ.0 .or. mgrank==1) .AND. WIN.GT.0.0D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                FLVW(I,J,K)=FLVW(I,J,K)-SU*WIN*VV(I,J,K-1)-SC*FG
              ELSE
                FLVW(I,J,K)=FLVW(I,J,K)-FG
              ENDIF

C             * Gz*Nu*(dv/dz+dw/dy)
C             ? 線形補間
              GG=YY(6,J)*(YY(2,J-1)*GGZ(I,J,K)+YY(2,J)*GGZ(I,J-1,K))
              FG=YY(6,J)*(YY(2,J)*BR+YY(2,J-1)*BL)
              IF     (NF(I,J,K  ).EQ.-1) THEN
                ANR=ANU(I,J,K-1)
              ELSEIF (NF(I,J,K-1).EQ.-1) THEN
                ANR=ANU(I,J,K  )
              ELSE
                ANR=ZZ(6,K)*( ZZ(2,K-1)*ANU(I,J,K  )
     &                       +ZZ(2,K  )*ANU(I,J,K-1))
              ENDIF
              IF     (NF(I,J-1,K  ).EQ.-1) THEN
                ANL=ANU(I,J-1,K-1)
              ELSEIF (NF(I,J-1,K-1).EQ.-1) THEN
                ANL=ANU(I,J-1,K  )
              ELSE
                ANL=ZZ(6,K)*( ZZ(2,K-1)*ANU(I,J-1,K  )
     &                       +ZZ(2,K  )*ANU(I,J-1,K-1))
              ENDIF
              AN=YY(6,J)*(YY(2,J-1)*ANR+YY(2,J)*ANL)
              IF (IT.EQ.0) THEN
                D1=ZZ(4,K  )*(VV(I,J,K  )-FG)*2.0D0
              ELSE
                D1=ZZ(4,K-1)*(FG-VV(I,J,K-1))*2.0D0
              ENDIF
              D2=YY(5,J)*(WW(I,J,K)-WW(I,J-1,K))
              FLVW(I,J,K)=FLVW(I,J,K)+GG*AN*(D1+D2)

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
