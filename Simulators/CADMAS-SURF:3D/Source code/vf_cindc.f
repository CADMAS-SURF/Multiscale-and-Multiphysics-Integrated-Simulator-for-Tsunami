      SUBROUTINE VF_CINDC(NF,INDC)

CD=== 概要 ===========================================================

CDT   VF_CINDC:セルの計算状態を示すインデックスINDCを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDC(@FOR-3D@) : I/O : I*4 : セルの計算状態を示すインデックス
      DIMENSION NF(NUMI,NUMJ,NUMK),INDC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      IA=1
      IB=NUMI
      JA=1
      JB=NUMJ
      IF (MYMIS.EQ.1) IA=2
      IF (MYMIE.EQ.1) IB=NUMI-1
      IF (MYMJS.EQ.1) JA=2
      IF (MYMJE.EQ.1) JB=NUMJ-1

CD    -- INDCを設定 --
      DO 120 K=2,NUMK-1
        DO 110 J=JA,JB
          DO 100 I=IA,IB
C           * 流体セルが最も多いと仮定
            IF     (NF(I,J,K).EQ. 0) THEN
              INDC(I,J,K)=0
C           * 気体セルが次に多いと仮定
            ELSEIF (NF(I,J,K).EQ. 8) THEN
              INDC(I,J,K)=-1
C           * そして、障害物セルならば
            ELSEIF (NF(I,J,K).EQ.-1) THEN
              INDC(I,J,K)=-1
C           * でなければ、表面セル
            ELSE
              INDC(I,J,K)=0
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
