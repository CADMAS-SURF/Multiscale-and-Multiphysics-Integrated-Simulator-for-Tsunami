      SUBROUTINE VF_VGENE(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                    RHOG,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                    CD0,GGV,GGX,GGY,GGZ,GGV1,AK,TT,CC,QU,QV,QW,
     &                    NF,INDX,INDY,INDZ,INDC)

CD=== 概要 ===========================================================

CDT   VF_VGENE:流速に関する生成消滅項を計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)   : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)   : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)   : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)   : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
CD    FX(@FOR-3D@)   : IN  : R*8 : x方向スタッガードセルでのVOF関数Fx
CD    FY(@FOR-3D@)   : IN  : R*8 : y方向スタッガードセルでのVOF関数Fy
CD    FZ(@FOR-3D@)   : IN  : R*8 : z方向スタッガードセルでのVOF関数Fz
CD    CD0(@FOR-3D@)  : IN  : R*8 : 抵抗係数
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率(GGVまたはGGV0)
CD    GGV1(@FOR-3D@) : IN  : R*8 : 空隙率(必ずGGV)
CD    GGX(@FOR-3D@)  : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)  : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)  : IN  : R*8 : z方向面積透過率
CD    AK(@FOR-3D@)   : IN  : R*8 : 乱流エネルギ
CD    TT(@FOR-3D@)   : IN  : R*8 : 温度
CD    CC(@FOR-3D@,LEQC) : IN  : R*8 : 濃度
CD    QU(@FOR-3D@)   : I/O : R*8 : x方向流速の生成消滅
CD    QV(@FOR-3D@)   : I/O : R*8 : y方向流速の生成消滅
CD    QW(@FOR-3D@)   : I/O : R*8 : z方向流速の生成消滅
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@) : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@) : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@) : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@) : IN  : I*4 : セルの計算状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),FX  (NUMI,NUMJ,NUMK)
      DIMENSION FY  (NUMI,NUMJ,NUMK),FZ  (NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION RHOG(NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION CD0 (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GGV1(NUMI,NUMJ,NUMK)
      DIMENSION TT  (NUMI,NUMJ,NUMK),AK  (NUMI,NUMJ,NUMK)
      DIMENSION CC  (NUMI,NUMJ,NUMK,LEQC)
      DIMENSION QU  (NUMI,NUMJ,NUMK),QV  (NUMI,NUMJ,NUMK)
      DIMENSION QW  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=IA+1
      IF (MYMJS.EQ.1) JA=JA+1

CAKIY -- 造波ソースのための定数 --

CD    -- 定数の計算 --
      WK=2.0D0/3.0D0
      DTI=1.0D0/DTNOW

CD    -- x方向流速の生成消滅項の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=IA,IB
            I1=I-1
C2F         IF (INDX(I,J,K).EQ.0 .AND. NF(I1,J,K)*NF(I,J,K).EQ.0) THEN
            IF (INDX(I,J,K).EQ.0 .AND.
     &                  INDC(I1,J,K)*INDC(I,J,K).EQ.0) THEN

C             * 密度の逆数の計算 *
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!            RI=1.0D0/(FX(I,J,K)*RHO0(1)+(1.0D0-FX(I,J,K))*RHO0(2))
              RHO02 = XX(6,I)*( XX(2,I )*RHOG(I ,J,K)
     &                         +XX(2,I1)*RHOG(I1,J,K))
              RI=1.0D0/(FX(I,J,K)*RHO0(1)+(1.0D0-FX(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C             * 面積補間する
              GV =XX(6,I)*(XX(2,I)*GGV (I,J,K)+XX(2,I1)*GGV (I1,J,K))
              GV1=XX(6,I)*(XX(2,I)*GGV1(I,J,K)+XX(2,I1)*GGV1(I1,J,K))
              CD =XX(6,I)*(XX(2,I)*CD0 (I,J,K)+XX(2,I1)*CD0 (I1,J,K))

CD            * -Gv/rho*dp/dxの計算
              QU(I,J,K)=QU(I,J,K)
     &                    +GV*RI*XX(5,I)*(PP(I1,J,K)-PP(I,J,K))
              IF (LEQK.NE.0) THEN
                QU(I,J,K)=QU(I,J,K)
     &                           +GV*WK*XX(5,I)*(AK(I1,J,K)-AK(I,J,K))
              ENDIF

CD            * -Rxの計算
              U=UU(I,J,K)
              V=XX(6,I)*( XX(2,I1)*(VV(I ,J,K)+VV(I ,J+1,K))
     &                   +XX(2,I )*(VV(I1,J,K)+VV(I1,J+1,K)))*0.5D0
              W=XX(6,I)*( XX(2,I1)*(WW(I ,J,K)+WW(I ,J,K+1))
     &                   +XX(2,I )*(WW(I1,J,K)+WW(I1,J,K+1)))*0.5D0
              UVW=SQRT(U*U+V*V+W*W)
              QU(I,J,K)=QU(I,J,K)
     &                  -0.5D0*CD*XX(5,I)*(1.0D0-GGX(I,J,K))*U*UVW

CD            * u*(Gv/dt)の計算 (移動構造物用)
              QU(I,J,K)=QU(I,J,K)+(GV1-GV)*DTI*U

CAKIY         * 造波ソース

            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- y方向流速の生成消滅項の計算 --
      DO 220 K=2,NUMK-1
        DO 210 J=JA,JB
          J1=J-1
          DO 200 I=MYIS,MYIE
C2F         IF (INDY(I,J,K).EQ.0 .AND. NF(I,J1,K)*NF(I,J,K).EQ.0) THEN
            IF (INDY(I,J,K).EQ.0 .AND.
     &                  INDC(I,J1,K)*INDC(I,J,K).EQ.0) THEN

C             * 密度の逆数の計算 *
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!            RI=1.0D0/(FY(I,J,K)*RHO0(1)+(1.0D0-FY(I,J,K))*RHO0(2))
              RHO02 = YY(6,J)*( YY(2,J )*RHOG(I,J ,K)
     &                         +YY(2,J1)*RHOG(I,J1,K))
              RI=1.0D0/(FY(I,J,K)*RHO0(1)+(1.0D0-FY(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C             * 面積補間する
              GV =YY(6,J)*(YY(2,J)*GGV (I,J,K)+YY(2,J1)*GGV (I,J1,K))
              GV1=YY(6,J)*(YY(2,J)*GGV1(I,J,K)+YY(2,J1)*GGV1(I,J1,K))
              CD =YY(6,J)*(YY(2,J)*CD0 (I,J,K)+YY(2,J1)*CD0 (I,J1,K))

CD            * -Gv/rho*dp/dyの計算
              QV(I,J,K)=QV(I,J,K)
     &                    +GV*RI*YY(5,J)*(PP(I,J1,K)-PP(I,J,K))
              IF (LEQK.NE.0) THEN
                QV(I,J,K)=QV(I,J,K)
     &                           +GV*WK*YY(5,J)*(AK(I,J1,K)-AK(I,J,K))
              ENDIF

CD            * -Ryの計算
              U=YY(6,J)*( YY(2,J1)*(UU(I,J ,K)+UU(I+1,J ,K))
     &                   +YY(2,J )*(UU(I,J1,K)+UU(I+1,J1,K)))*0.5D0
              V=VV(I,J,K)
              W=YY(6,J)*( YY(2,J1)*(WW(I,J ,K)+WW(I,J ,K+1))
     &                   +YY(2,J )*(WW(I,J1,K)+WW(I,J1,K+1)))*0.5D0
              UVW=SQRT(U*U+V*V+W*W)
              QV(I,J,K)=QV(I,J,K)
     &                  -0.5D0*CD*YY(5,J)*(1.0D0-GGY(I,J,K))*V*UVW

CD            * v*(Gv/dt)の計算 (移動構造物用)
              QV(I,J,K)=QV(I,J,K)+(GV1-GV)*DTI*V

CD            * 造波ソース

            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- z方向流速の生成消滅項の計算 --
      DO 330 K=3,NUMK-1
        K1=K-1
        DO 320 J=MYJS,MYJE
          DO 310 I=MYIS,MYIE
C2F         IF (INDZ(I,J,K).EQ.0 .AND. NF(I,J,K1)*NF(I,J,K).EQ.0) THEN
            IF (INDZ(I,J,K).EQ.0 .AND.
     &                  INDC(I,J,K1)*INDC(I,J,K).EQ.0) THEN

C             * 密度の逆数の計算 *
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!            RI=1.0D0/(FZ(I,J,K)*RHO0(1)+(1.0D0-FZ(I,J,K))*RHO0(2))
              RHO02 = ZZ(6,K)*( ZZ(2,K )*RHOG(I,J,K )
     &                         +ZZ(2,K1)*RHOG(I,J,K1))
              RI=1.0D0/(FZ(I,J,K)*RHO0(1)+(1.0D0-FZ(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C             * 面積補間する
              GV =ZZ(6,K)*(ZZ(2,K)*GGV (I,J,K)+ZZ(2,K1)*GGV (I,J,K1))
              GV1=ZZ(6,K)*(ZZ(2,K)*GGV1(I,J,K)+ZZ(2,K1)*GGV1(I,J,K1))
              CD =ZZ(6,K)*(ZZ(2,K)*CD0 (I,J,K)+ZZ(2,K1)*CD0 (I,J,K1))

CD            * -Gv/rho*dp/dzの計算
              QW(I,J,K)=QW(I,J,K)
     &                    +GV*RI*ZZ(5,K)*(PP(I,J,K1)-PP(I,J,K))
              IF (LEQK.NE.0) THEN
                QW(I,J,K)=QW(I,J,K)
     &                           +GV*WK*ZZ(5,K)*(AK(I,J,K1)-AK(I,J,K))
              ENDIF

CD            * -Rzの計算
              U=ZZ(6,K)*( ZZ(2,K1)*(UU(I,J,K )+UU(I+1,J,K ))
     &                   +ZZ(2,K )*(UU(I,J,K1)+UU(I+1,J,K1)))*0.5D0
              V=ZZ(6,K)*( ZZ(2,K1)*(VV(I,J,K )+VV(I,J+1,K ))
     &                   +ZZ(2,K )*(VV(I,J,K1)+VV(I,J+1,K1)))*0.5D0
              W=WW(I,J,K)
              UVW=SQRT(U*U+V*V+W*W)
              QW(I,J,K)=QW(I,J,K)
     &                  -0.5D0*CD*ZZ(5,K)*(1.0D0-GGZ(I,J,K))*W*UVW

CD            * w*(Gv/dt)の計算 (移動構造物用)
              QW(I,J,K)=QW(I,J,K)+(GV1-GV)*DTI*W

CD            * 造波ソース

CD            * -Gv*gzの計算
              QW(I,J,K)=QW(I,J,K)-GV*GRZ0

CD            * -Gv*gz(dr/r)の計算
              DR=0.0D0
              IF (LEQT.NE.0) THEN
                T=ZZ(6,K)*(ZZ(2,K)*TT(I,J,K1)+ZZ(2,K1)*TT(I,J,K))
                DR=DR-TDR0*(T-TDT0)
              ENDIF
              DO 300 LC=1,LEQC
                C=ZZ(6,K)*(ZZ(2,K)*CC(I,J,K1,LC)+ZZ(2,K1)*CC(I,J,K,LC))
                DR=DR-CDR0(LC)*(C-CDC0(LC))
 300          CONTINUE
C2F           QW(I,J,K)=QW(I,J,K)-GV*GRZ0*DR/RHO0
              QW(I,J,K)=QW(I,J,K)-GV*GRZ0*DR/RHO0(1)

            ENDIF
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
