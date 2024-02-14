      SUBROUTINE VF_VPDRDT(AD,ALI,ALJ,ALK,AUI,AUJ,AUK,BB
     &                    ,RHOG,RHOGO,DRHODP,DRHODT
     &                    ,UU,VV,WW,FF
     &                    ,GGV,GGX,GGY,GGZ,GLV,XX,YY,ZZ,NF,INDC)

CD=== 概要 ===========================================================

CDT   VF_VPDRDT:ポテンシャル関数の連立1次方程式の密度変化による項

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    AD(@FOR-3D@)      : OUT : R*8 : 非対称行列Aの対角成分
CD    ALI(@FOR-3D@)     : OUT : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(@FOR-3D@)     : OUT : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(@FOR-3D@)     : OUT : R*8 : 非対称行列AのK-1に関する成分
CD    AUI(@FOR-3D@)     : OUT : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(@FOR-3D@)     : OUT : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(@FOR-3D@)     : OUT : R*8 : 非対称行列AのK+1に関する成分
CD    BB(@FOR-3D@)      : OUT : R*8 : 非対称連立1次方程式の右辺
CD    RHOG(@FOR-3D@)    : IN : R*8 : 気相密度
CD    RHOGO(@FOR-3D@)   : IN : R*8 : 前時刻の気相密度
CD    DRHODP(@FOR-3D@)  : IN : R*8 : (dρ/dp)/ρ
CD    DRHODT(@FOR-3D@)  : IN : R*8 : (dρ/dt)/ρ
CD    UU(@FOR-3D@)      : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)      : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)      : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    GLV(@FOR-3D@)     : IN  : R*8 : =GGV+(1-GGV)*CM
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDC(@FOR-3D@)    : IN  : I*4 : セルの計算状態を示すインデックス

      DIMENSION AD  (NUMI,NUMJ,NUMK),ALI (NUMI,NUMJ,NUMK)
      DIMENSION ALJ (NUMI,NUMJ,NUMK),ALK (NUMI,NUMJ,NUMK)
      DIMENSION AUI (NUMI,NUMJ,NUMK),AUJ (NUMI,NUMJ,NUMK)
      DIMENSION AUK (NUMI,NUMJ,NUMK),BB  (NUMI,NUMJ,NUMK)
      DIMENSION RHOG  (NUMI,NUMJ,NUMK)
      DIMENSION RHOGO (NUMI,NUMJ,NUMK)
      DIMENSION DRHODP(NUMI,NUMJ,NUMK)
      DIMENSION DRHODT(NUMI,NUMJ,NUMK)
      DIMENSION UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK)
      DIMENSION WW(NUMI,NUMJ,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
      DIMENSION GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
      DIMENSION GLV(NUMI,NUMJ,NUMK)
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

      IF(ISTATE.EQ.0) RETURN
C
C  div v + (1-F)*(dρ/dt)/ρ = 0 の (1-F)*(dρ/dt)/ρ をpoisson方程式に加える
C
C  ρ^new = ρ^old + (dρ/dp)^old*Δp
C
C  (dρ/dt)^new/ρ^new = (dρ/dt)^new / [ρ^old*(1 + (dρ/dp)^old*Δp/ρ^old)]
C                      = (dρ/dt)^new / ρ^old * (1 - (dρ/dp)^old*Δp/ρ^old)
C                      = (dρ/dt)^new / ρ^old
C                       -(dρ/dt)^new / ρ^old*(dρ/dp)^old*Δp/ρ^old
C                      = (dρ/dt)^new / ρ^old
C                       -(dρ/dt)^old*(dρ/dp)^old/(ρ^old)^2 * Δp
C
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF(NF(I,J,K).GT.0) THEN
C
              AVL  = (1.0D0 - FF(I,J,K))*XX(2,I)*YY(2,J)*ZZ(2,K)
              AVLT = AVL/DTNOW
C
              GV = GGV(I,J,K)/GLV(I,J,K)
              WUC = 0.5D0*( GGX(I+1,J,K)*UU(I+1,J,K)
     &                     +GGX(I  ,J,K)*UU(I  ,J,K))*GV
              WVC = 0.5D0*( GGY(I,J+1,K)*VV(I,J+1,K)
     &                     +GGY(I,J  ,K)*VV(I,J  ,K))*GV
              WWC = 0.5D0*( GGZ(I,J,K+1)*WW(I,J,K+1)
     &                     +GGZ(I,J,K  )*WW(I,J,K  ))*GV
C
              IF(WUC.GT.0.0D0) THEN
                IF(NF(I-1,J,K).GT.0) THEN
C                 DRHODX = (RHOG(I,J,K) - RHOG(I-1,J,K))*XX(5,I)
                  DCOEF = AVLT*WUC*XX(5,I)
                  AD (I,J,K) = AD (I,J,K) + DCOEF*DRHODP(I  ,J,K)
                  ALI(I,J,K) = ALI(I,J,K) - DCOEF*DRHODP(I-1,J,K)
                END IF
              ELSE
                IF(NF(I+1,J,K).GT.0) THEN
C                 DRHODX = (RHOG(I+1,J,K) - RHOG(I,J,K))*XX(5,I+1)
                  DCOEF = AVLT*WUC*XX(5,I+1)
                  AD (I,J,K) = AD (I,J,K) - DCOEF*DRHODP(I  ,J,K)
                  AUI(I,J,K) = AUI(I,J,K) + DCOEF*DRHODP(I-1,J,K)
                END IF
              END IF
C
              IF(WVC.GT.0.0D0) THEN
                IF(NF(I,J-1,K).GT.0) THEN
C                 DRHODY = (RHOG(I,J,K) - RHOG(I,J-1,K))*YY(5,J)
                  DCOEF = AVLT*WVC*YY(5,J)
                  AD (I,J,K) = AD (I,J,K) + DCOEF*DRHODP(I,J  ,K)
                  ALJ(I,J,K) = ALJ(I,J,K) - DCOEF*DRHODP(I,J-1,K)
                END IF
              ELSE
                IF(NF(I,J+1,K).GT.0) THEN
C                 DRHODY = (RHOG(I,J+1,K) - RHOG(I,J,K))*YY(5,J+1)
                  DCOEF = AVLT*WVC*YY(5,J+1)
                  AD (I,J,K) = AD (I,J,K) - DCOEF*DRHODP(I,J  ,K)
                  AUJ(I,J,K) = AUJ(I,J,K) + DCOEF*DRHODP(I,J+1,K)
                END IF
              END IF
C
              IF(WWC.GT.0.0D0) THEN
                IF(NF(I,J,K-1).GT.0) THEN
C                 DRHODZ = (RHOG(I,J,K) - RHOG(I,J,K-1))*ZZ(5,K)
                  DCOEF = AVLT*WWC*ZZ(5,K)
                  AD (I,J,K) = AD (I,J,K) + DCOEF*DRHODP(I,J,K  )
                  ALK(I,J,K) = ALK(I,J,K) - DCOEF*DRHODP(I,J,K-1)
                END IF
              ELSE
                IF(NF(I,J,K+1).GT.0) THEN
C                 DRHODZ = (RHOG(I,J,K+1) - RHOG(I,J,K))*ZZ(5,K+1)
                  DCOEF = AVLT*WWC*ZZ(5,K+1)
                  AD (I,J,K) = AD (I,J,K) - DCOEF*DRHODP(I,J,K  )
                  AUK(I,J,K) = AUK(I,J,K) + DCOEF*DRHODP(I,J,K+1)
                END IF
              END IF
C
              AD(I,J,K) = AD(I,J,K) + AVLT*DRHODP(I,J,K)/DTNOW
              AD(I,J,K) = AD(I,J,K) - AVLT*DRHODP(I,J,K)*DRHODT(I,J,K)
              BB(I,J,K) = BB(I,J,K) + AVL*DRHODT(I,J,K)
            END IF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
