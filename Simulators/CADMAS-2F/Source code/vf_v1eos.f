      SUBROUTINE VF_V1EOS(RHOG,DRHODP,DRHODT,DBUF
     &                   ,UU,VV,WW,PP,RHOGO
     &                   ,GGV,GGX,GGY,GGZ,GLV,XX,YY,ZZ,NF,INDC)

CD=== 概要 ===========================================================

CDT   VF_V1EOS:状態方程式 rho=rho(p)

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
CD    RHOG(@FOR-3D@)    : OUT : R*8 : 気相密度
CD    DRHODP(@FOR-3D@)  : OUT : R*8 : (dρ/dp)/ρ
CD    DRHODT(@FOR-3D@)  : OUT : R*8 : (dρ/dt)/ρ
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    UU(@FOR-3D@)      : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)      : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)      : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)      : IN  : R*8 : 圧力
CD    RHOGO(@FOR-3D@)   : IN  : R*8 : 前時刻の気相密度
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
      DIMENSION RHOG(NUMI,NUMJ,NUMK)
      DIMENSION DRHODP(NUMI,NUMJ,NUMK)
      DIMENSION DRHODT(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK)
      DIMENSION WW(NUMI,NUMJ,NUMK),PP(NUMI,NUMJ,NUMK)
      DIMENSION RHOGO(NUMI,NUMJ,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
      DIMENSION GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
      DIMENSION GLV(NUMI,NUMJ,NUMK)
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK)
C==== 実行 ===========================================================

      RHOG  (:,:,:) = RHO0(2)
      DRHODP(:,:,:) = 0.0D0
      DRHODT(:,:,:) = 0.0D0

      IF(ISTATE.EQ.0) RETURN

      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF(INDC(I,J,K).NE.-1) THEN
              CALL USER_EOS(ISTATE,SPARAM,PP(I,J,K)
     &                     ,RHOG(I,J,K),DRHODP(I,J,K))
            END IF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD1(RHOG  ,DBUF,0)
      CALL VF_P3SRD1(DRHODP,DBUF,0)
C
C 表面セルの気相密度は、気体セルから埋め込む
C
      DO K=2,NUMK-1
        DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            IF     (NF(I,J,K).EQ.1) THEN
              RHOG(I,J,K)   = RHOG  (I-1,J,K)
              DRHODP(I,J,K) = DRHODP(I-1,J,K)
            ELSE IF(NF(I,J,K).EQ.2) THEN
              RHOG(I,J,K)   = RHOG  (I+1,J,K)
              DRHODP(I,J,K) = DRHODP(I+1,J,K)
            ELSE IF(NF(I,J,K).EQ.3) THEN
              RHOG(I,J,K)   = RHOG  (I,J-1,K)
              DRHODP(I,J,K) = DRHODP(I,J-1,K)
            ELSE IF(NF(I,J,K).EQ.4) THEN
              RHOG(I,J,K)   = RHOG  (I,J+1,K)
              DRHODP(I,J,K) = DRHODP(I,J+1,K)
            ELSE IF(NF(I,J,K).EQ.5) THEN
              RHOG(I,J,K)   = RHOG  (I,J,K-1)
              DRHODP(I,J,K) = DRHODP(I,J,K-1)
            ELSE IF(NF(I,J,K).EQ.6) THEN
              RHOG(I,J,K)   = RHOG  (I,J,K+1)
              DRHODP(I,J,K) = DRHODP(I,J,K+1)
            END IF
          END DO
        END DO
      END DO
C
C 気体セルと表面セルで dρ/dt を計算
C
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            IF(NF(I,J,K).GT.0) THEN
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
                  DRHODX = (RHOG(I,J,K) - RHOG(I-1,J,K))*XX(5,I)
                ELSE
                  DRHODX = 0.0D0
                END IF
              ELSE
                IF(NF(I+1,J,K).GT.0) THEN
                  DRHODX = (RHOG(I+1,J,K) - RHOG(I,J,K))*XX(5,I+1)
                ELSE
                  DRHODX = 0.0D0
                END IF
              END IF
C
              IF(WVC.GT.0.0D0) THEN
                IF(NF(I,J-1,K).GT.0) THEN
                  DRHODY = (RHOG(I,J,K) - RHOG(I,J-1,K))*YY(5,J)
                ELSE
                  DRHODY = 0.0D0
                END IF
              ELSE
                IF(NF(I,J+1,K).GT.0) THEN
                  DRHODY = (RHOG(I,J+1,K) - RHOG(I,J,K))*YY(5,J+1)
                ELSE
                  DRHODY = 0.0D0
                END IF
              END IF
C
              IF(WWC.GT.0.0D0) THEN
                IF(NF(I,J,K-1).GT.0) THEN
                  DRHODZ = (RHOG(I,J,K) - RHOG(I,J,K-1))*ZZ(5,K)
                ELSE
                  DRHODZ = 0.0D0
                END IF
              ELSE
                IF(NF(I,J,K+1).GT.0) THEN
                  DRHODZ = (RHOG(I,J,K+1) - RHOG(I,J,K))*ZZ(5,K+1)
                ELSE
                  DRHODZ = 0.0D0
                END IF
              END IF
C
C  (1-F)*(dρ/dt)/ρ [1/s]
C
              DRHODT(I,J,K) =
     &         (  GGV(I,J,K)*(RHOG(I,J,K) - RHOGO(I,J,K))/DTNOW
     &          + WUC*DRHODX + WVC*DRHODY + WWC*DRHODZ)/RHOG(I,J,K)
            END IF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
   
      CALL VF_P3SRD1(DRHODT,DBUF,0)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
