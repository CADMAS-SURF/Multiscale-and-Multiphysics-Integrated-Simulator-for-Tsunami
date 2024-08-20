      SUBROUTINE VF_CNUT0(AK,AE,ANUT,NF)

CD=== 概要 ===========================================================

CDT   VF_CNUT0:渦動粘性係数を計算する(k-ε2方程式モデル)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    AK(@FOR-3D@)   : IN  : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)   : IN  : R*8 : 乱流エネルギ散逸
CD    ANUT(@FOR-3D@) : I/O : R*8 : 渦動粘性係数νt
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION AK  (NUMI,NUMJ,NUMK),AE(NUMI,NUMJ,NUMK)
      DIMENSION ANUT(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

      IF (LEQK.EQ.0) CALL VF_A2ERR('VF_CNUT0','P.G ERROR.')

CD    -- 並列時の範囲変更 --
      IA=1
      IB=NUMI
      JA=1
      JB=NUMJ
      IF (MYMIS.EQ.1) IA=2
      IF (MYMIE.EQ.1) IB=NUMI-1
      IF (MYMJS.EQ.1) JA=2
      IF (MYMJE.EQ.1) JB=NUMJ-1

CD    -- 渦動粘性係数νt=Cμ・k2/εを計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=JA,JB
          DO 100 I=IA,IB
            IF (NF(I,J,K).NE.-1) THEN
              W=MAX(AE(I,J,K),AKMINE)
              ANUT(I,J,K)=AKCMU*AK(I,J,K)*AK(I,J,K)/W
            ELSE
              ANUT(I,J,K)=0.0D0
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
