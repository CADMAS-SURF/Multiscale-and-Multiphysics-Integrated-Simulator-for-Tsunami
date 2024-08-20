      SUBROUTINE VF_CNFDFL(NF)

CD=== 概要 ===========================================================

CDT   VF_CNFDLF:NFへデフォルト値を設定する
CD      (1)仮想セルを障害物セル(-1)とする
CD      (2)内部セルを流体セル(0)とする

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@) : OUT : I*4 : セルの状態を示すインデックス
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 全てを流体セルとする --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            NF(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120   CONTINUE

CD    -- 仮想セルを障害物セルとする --
      DO 210 J=1,NUMJ
        DO 200 I=1,NUMI
          NF(I,J,1   )=-1
          NF(I,J,NUMK)=-1
 200    CONTINUE
 210  CONTINUE
      DO 310 K=1,NUMK
        DO 300 I=1,NUMI
          IF (MYGJS.EQ.1    ) NF(I,1   ,K)=-1
          IF (MYGJE.EQ.NUMJ0) NF(I,NUMJ,K)=-1
 300    CONTINUE
 310  CONTINUE
      DO 410 K=1,NUMK
        DO 400 J=1,NUMJ
          IF (MYGIS.EQ.1    ) NF(1   ,J,K)=-1
          IF (MYGIE.EQ.NUMI0) NF(NUMI,J,K)=-1
 400    CONTINUE
 410  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
