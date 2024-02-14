      SUBROUTINE VF_CGLV(CM0,GGV,GLV,NF)

CD=== 概要 ===========================================================

CDT   VF_CGLV:時間依存型のGLVを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    CM0(@FOR-3D@)  : IN  : R*8 : 慣性力係数
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率
CD    GLV(@FOR-3D@)  : OUT : R*8 : =GGV+(1-GGV)*CM
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION CM0(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
      DIMENSION GLV(NUMI,NUMJ,NUMK)
      DIMENSION NF (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- GLVを設定する --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            IF (NF(I,J,K).EQ.-1) THEN
              GLV(I,J,K)=1.0D0
            ELSE
              GLV(I,J,K)=GGV(I,J,K)+(1.0D0-GGV(I,J,K))*CM0(I,J,K)
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
