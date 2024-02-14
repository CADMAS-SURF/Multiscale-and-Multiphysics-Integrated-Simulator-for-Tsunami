      SUBROUTINE VF_MZFRWD(DD,ALI,ALJ,ALK,X,Y,INDC,MI,MJ,MK,NI,NJ,NK)

CD=== 概要 ===========================================================

CDT   VF_MZFRWD:前進代入で(L*DINV+I)*x=yを解く

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DD(MI,MJ,MK)   : IN  : R*8 : 前処理用対角行列(逆数)
CD    ALI(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのK-1に関する成分
CD    X(MI,MJ,MK)    : OUT : R*8 : ベクトルX(解)
CD    Y(MI,MJ,MK)    : IN  : R*8 : ベクトルY(右辺)
CD    INDC(MI,MJ,MK) : IN  : I*4 : インデックス
CD    MI             : IN  : I*4 : x方向最大格子数+1
CD    MJ             : IN  : I*4 : y方向最大格子数+1
CD    MK             : IN  : I*4 : z方向最大格子数+1
CD    NI             : IN  : I*4 : x方向格子数+1
CD    NJ             : IN  : I*4 : y方向格子数+1
CD    NK             : IN  : I*4 : z方向格子数+1
      DIMENSION DD  (MI,MJ,MK),ALI (MI,MJ,MK),ALJ (MI,MJ,MK)
      DIMENSION ALK (MI,MJ,MK),X   (MI,MJ,MK),Y   (MI,MJ,MK)
      DIMENSION INDC(MI,MJ,MK)

C==== 実行 ===========================================================

CD    -- (L*DINV+I)*x=yを解く --
      DO 120 K=2,NK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
              X(I,J,K)=Y(I,J,K)
     &                 -ALI(I,J,K)*DD(I-1,J  ,K  )*X(I-1,J  ,K  )
     &                 -ALJ(I,J,K)*DD(I  ,J-1,K  )*X(I  ,J-1,K  )
     &                 -ALK(I,J,K)*DD(I  ,J  ,K-1)*X(I  ,J  ,K-1)
            ENDIF
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
