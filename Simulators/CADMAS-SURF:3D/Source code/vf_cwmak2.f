      SUBROUTINE VF_CWMAK2(WVT,ZC,WVUN,WVUT,IWVTYP)

CD=== 概要 ===========================================================

CDT   VF_CWMAK2:造波のための流速の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    WVT    : IN  : R*8 : 無次元位相
CD    ZC     : IN  : R*8 : 鉛直座標(静面を基準)
CD    WVUN   : OUT : R*8 : 法線方向流速
CD    WVUT   : OUT : R*8 : 接線方向流速
CD    IWVTYP : IN  : I*4 : 造波波形の種別
CD                         =-2:Stokes波(第5次近似解)
CD                         =-1:Cnoidal波(第3次近似解)
CD                         > 0:流れ関数法Bとその次数

CD    -- 局所変数 --
      REAL*4 R4TN,R4Z,R4U,R4W,R4P,R4UT,R4WT

C==== 実行 ===========================================================

CD    -- 流速を求める --
      R4TN=REAL(WVT)
      IF     (IWVTYP.EQ.-2) THEN
        R4Z =REAL(ZC*100.0D0)
        CALL VF_STK2(R4TN,R4Z,R4U,R4W,R4P)
        WVUN=DBLE(R4U)*0.01D0
        WVUT=DBLE(R4W)*0.01D0
      ELSEIF (IWVTYP.EQ.-1) THEN
        R4Z=REAL(ZC*100.0D0)
        CALL VF_CND2(R4TN,R4Z,R4U,R4W,R4P)
        WVUN=DBLE(R4U)*0.01D0
        WVUT=DBLE(R4W)*0.01D0
      ELSEIF (IWVTYP.GE. 1) THEN
        R4Z=REAL(ZC)
        CALL vf_sfmb2(R4TN,R4Z,R4U,R4W,R4UT,R4WT,R4P)
        WVUN=DBLE(R4U)
        WVUT=DBLE(R4W)
      ELSE
        CALL VF_A2ERR('VF_CWMAK2','P.G ERROR.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
