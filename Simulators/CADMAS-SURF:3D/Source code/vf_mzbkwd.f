      SUBROUTINE VF_MZBKWD(DD,AUI,AUJ,AUK,X,Y,INDC,MI,MJ,MK,NI,NJ,NK)

CD=== 概要 ===========================================================

CDT   VF_MZBKWD:後退代入で(D+U)*x=yを解く

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DD(MI,MJ,MK)   : IN  : R*8 : 前処理用対角行列(逆数)
CD    AUI(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのK+1に関する成分
CD    X(MI,MJ,MK)    : OUT : R*8 : ベクトルX(解)
CD    Y(MI,MJ,MK)    : IN  : R*8 : ベクトルY(右辺)
CD    INDC(MI,MJ,MK) : IN  : I*4 : インデックス
CD    MI             : IN  : I*4 : x方向最大格子数+1
CD    MJ             : IN  : I*4 : y方向最大格子数+1
CD    MK             : IN  : I*4 : z方向最大格子数+1
CD    NI             : IN  : I*4 : x方向格子数+1
CD    NJ             : IN  : I*4 : y方向格子数+1
CD    NK             : IN  : I*4 : z方向格子数+1
      DIMENSION DD  (MI,MJ,MK),AUI (MI,MJ,MK),AUJ (MI,MJ,MK)
      DIMENSION AUK (MI,MJ,MK),X   (MI,MJ,MK),Y   (MI,MJ,MK)
      DIMENSION INDC(MI,MJ,MK)

C==== 実行 ===========================================================

CD    -- (D+U)*x=yを解く --
      DO 120 K=NK-1,2,-1
        DO 110 J=MYJE,MYJS,-1
          DO 100 I=MYIE,MYIS,-1
            IF (INDC(I,J,K).NE.-1) THEN
              X(I,J,K)=(Y(I,J,K)-AUI(I,J,K)*X(I+1,J  ,K  )
     &                          -AUJ(I,J,K)*X(I  ,J+1,K  )
     &                          -AUK(I,J,K)*X(I  ,J  ,K+1))*DD(I,J,K)
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
