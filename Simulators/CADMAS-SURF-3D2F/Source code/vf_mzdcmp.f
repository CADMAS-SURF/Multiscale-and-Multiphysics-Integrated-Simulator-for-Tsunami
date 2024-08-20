      SUBROUTINE VF_MZDCMP(AD,ALI,ALJ,ALK,AUI,AUJ,AUK,DD,CGPARA,
     &                     INDC,MI,MJ,MK,NI,NJ,NK,ICGTYP,ISW)

CD=== 概要 ===========================================================

CDT   VF_MZDCMP:不完全LU分解により前処理用の対角行列DD(逆数)を求める

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    AD (MI,MJ,MK)  : IN  : R*8 : 非対称行列Aの対角成分
CD    ALI(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのK-1に関する成分
CD    AUI(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのK+1に関する成分
CD    DD (MI,MJ,MK)  : OUT : R*8 : 前処理用対角行列(逆数)
CD    CGPARA         : IN  : R*8 : MILU用パラメータ
CD    INDC(MI,MJ,MK) : IN  : I*4 : インデックス
CD    MI             : IN  : I*4 : x方向最大格子数+1
CD    MJ             : IN  : I*4 : y方向最大格子数+1
CD    MK             : IN  : I*4 : z方向最大格子数+1
CD    NI             : IN  : I*4 : x方向格子数+1
CD    NJ             : IN  : I*4 : y方向格子数+1
CD    NK             : IN  : I*4 : z方向格子数+1
CD    ICGTYP         : IN  : I*4 : 前処理の種類
CD                                 = 0:不完全LU分解(ILU)
CD                                 !=0:修正不完全LU分解(MILU)
CD    ISW            : OUT : I*4 : 不完全LU分解の成否
CD                                 = 0:失敗した
CD                                 !=0:成功した
      DIMENSION AD  (MI,MJ,MK),ALI (MI,MJ,MK),ALJ (MI,MJ,MK)
      DIMENSION ALK (MI,MJ,MK),AUI (MI,MJ,MK),AUJ (MI,MJ,MK)
      DIMENSION AUK (MI,MJ,MK),DD  (MI,MJ,MK)
      DIMENSION INDC(MI,MJ,MK)

C==== 実行 ===========================================================

CD    -- MILU用パラメータの設定 --
      U=0.0D0
      IF (ICGTYP.NE.0) U=CGPARA

CD    -- 不完全LU分解 --
      NPOS=1
      DO 120 K=2,NK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
              AU1=AUI(I-1,J  ,K  )+U*(AUJ(I-1,J  ,K  )+AUK(I-1,J  ,K  ))
              AU2=AUJ(I  ,J-1,K  )+U*(AUI(I  ,J-1,K  )+AUK(I  ,J-1,K  ))
              AU3=AUK(I  ,J  ,K-1)+U*(AUI(I  ,J  ,K-1)+AUJ(I  ,J  ,K-1))
              DD(I,J,K)=1.0D0/(AD(I,J,K)-ALI(I,J,K)*DD(I-1,J  ,K  )*AU1
     &                                  -ALJ(I,J,K)*DD(I  ,J-1,K  )*AU2
     &                                  -ALK(I,J,K)*DD(I  ,J  ,K-1)*AU3)
              IF (DD(I,J,K).LE.0.0D0) NPOS=0
            ENDIF
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE

CD    -- 前処理用対角行列のある成分が負 --
      ISW=1
      IF (NPOS.EQ.0) ISW=0

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
