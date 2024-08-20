      SUBROUTINE VF_PMGST2(XPF,YPF,ZPF,IPF,JPF,KPF)

CD=== 概要 ===========================================================

CDT   VF_PMGST2:マルチグリッド環境の子格子の数の設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
      DIMENSION :: XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION :: IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))

CD    -- 局所変数 --

C==== 実行 ===========================================================

      IF(MGPARE(MGRANK+1).EQ.0) GO TO 9999
      if( MGPINF(1).EQ.0.AND.MGPINF(2).EQ.0.AND.MGPINF(3).EQ.0 )
     $   GO TO 9999
C
C X 方向
      DO 100 II=0,MGPINF(1)
        IPF(II) = 0
 100  CONTINUE
C
      DO 110 I=MYIS,MYIE
        II = INT(0.5D0*(XPF(I)+XPF(I+1))) + 1
        IPF(II) = IPF(II) + 1
 110  CONTINUE
C
      IPF(0) = MYIS-1
      DO 120 II=1,MGPINF(1)
        IPF(II) = IPF(II) + IPF(II-1)
 120  CONTINUE
C
C Y 方向
      DO 200 JJ=0,MGPINF(2)
        JPF(JJ) = 0
 200  CONTINUE
C
      DO 210 J=MYJS,MYJE
        JJ = INT(0.5D0*(YPF(J)+YPF(J+1))) + 1
        JPF(JJ) = JPF(JJ) + 1
 210  CONTINUE
C
      JPF(0) = MYJS-1
      DO 220 JJ=1,MGPINF(2)
        JPF(JJ) = JPF(JJ) + JPF(JJ-1)
 220  CONTINUE
C
C Z 方向
      DO 300 KK=0,MGPINF(3)
        KPF(KK) = 0
 300  CONTINUE
C
      DO 310 K=2,NUMK-1
        KK = INT(0.5D0*(ZPF(K)+ZPF(K+1))) + 1
        KPF(KK) = KPF(KK) + 1
 310  CONTINUE
C
      KPF(0) = 1
      DO 320 KK=1,MGPINF(3)
        KPF(KK) = KPF(KK) + KPF(KK-1)
 320  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
