      SUBROUTINE VF_CWMAK1(WVT,WVZ,IWVTYP)

CD=== 概要 ===========================================================

CDT   VF_CWMAK1:造波のための水位の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    WVT    : IN  : R*8 : 無次元位相
CD    WVZ    : OUT : R*8 : 水位変動
CD    IWVTYP : IN  : I*4 : 造波波形の種別
CD                         =-2:Stokes波(第5次近似解)
CD                         =-1:Cnoidal波(第3次近似解)
CD                         > 0:流れ関数法Bとその次数

CD    -- 局所変数 --
      REAL*4 R4TN,R4Z,R4U,R4W,R4P,R4UT,R4WT

C==== 実行 ===========================================================

CD    -- 水位変動を求める --
      R4TN=REAL(WVT)
      IF     (IWVTYP.EQ.-2) THEN
        CALL VF_STK1(R4TN,R4Z)
        WVZ=DBLE(R4Z)*0.01D0
      ELSEIF (IWVTYP.EQ.-1) THEN
        CALL VF_CND1(R4TN,R4Z)
        WVZ=DBLE(R4Z)*0.01D0
      ELSEIF (IWVTYP.GE. 1) THEN
        CALL vf_sfmb1(R4TN,R4Z)
        WVZ=DBLE(R4Z)
C       * SFMB2のダミー初期化
        CALL vf_sfmb2(-1.0,0.0,R4U,R4W,R4UT,R4WT,R4P)
      ELSE
        CALL VF_A2ERR('VF_CWMAK1','P.G ERROR.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
