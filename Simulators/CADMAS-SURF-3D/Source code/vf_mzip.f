      SUBROUTINE VF_MZIP(X,Y,S,INDC,MI,MJ,MK,NI,NJ,NK)

CD=== 概要 ===========================================================

CDT   VF_MZIP:ベクトルXとベクトルYの内積Sを計算する(x*y=s)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    X(MI,MJ,MK)    : IN  : R*8 : ベクトルX
CD    Y(MI,MJ,MK)    : IN  : R*8 : ベクトルY
CD    S              : OUT : R*8 : 内積S
CD    INDC(MI,MJ,MK) : IN  : I*4 : インデックス
CD    MI             : IN  : I*4 : x方向最大格子数+1
CD    MJ             : IN  : I*4 : y方向最大格子数+1
CD    MK             : IN  : I*4 : z方向最大格子数+1
CD    NI             : IN  : I*4 : x方向格子数+1
CD    NJ             : IN  : I*4 : y方向格子数+1
CD    NK             : IN  : I*4 : z方向格子数+1
      DIMENSION X(MI,MJ,MK),Y(MI,MJ,MK),INDC(MI,MJ,MK)

C==== 実行 ===========================================================

CD    -- x*yを計算 --
      S=0.0D0
      DO 120 K=2,NK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
              S=S+X(I,J,K)*Y(I,J,K)
            ENDIF
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE
      W=S
      CALL VF_P1SUMD(W,S)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
