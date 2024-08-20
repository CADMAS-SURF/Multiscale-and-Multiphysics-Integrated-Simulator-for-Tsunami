      SUBROUTINE VF_CWMAK0(WVDPTH,WVPERI,WVHEIG,WVLENG,IWVTYP)

CD=== 概要 ===========================================================

CDT   VF_CWMAK0:造波のための関数の初期化

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    WVDPTH : IN  : R*8 : 水深
CD    WVPERI : IN  : R*8 : 周期
CD    WVHEIG : IN  : R*8 : 波高
CD    WVLENG : OUT : R*8 : 波長
CD    IWVTYP : IN  : I*4 : 造波波形の種別
CD                         =-2:Stokes波(第5次近似解)
CD                         =-1:Cnoidal波(第3次近似解)
CD                         > 0:流れ関数法Bとその次数

CD    -- 局所変数 --
      REAL*4 R4D,R4T,R4H,R4W1,R4E1,R4K,R4L,R4C,R4CC
      REAL*4 R4U,R4A(24),R4HH

C==== 実行 ===========================================================

CD    -- 初期化ルーチンを呼び出す --
      IF     (IWVTYP.EQ.-2) THEN
        R4D=REAL(WVDPTH*100.0D0)
        R4T=REAL(WVPERI)
        R4H=REAL(WVHEIG*100.0D0)
        N=5
        CALL VF_STK0(N,R4D,R4T,R4H,R4W1,R4E1,R4K,R4L,R4C,R4CC)
        WVLENG=DBLE(R4L)*0.01D0
      ELSEIF (IWVTYP.EQ.-1) THEN
        R4D=REAL(WVDPTH*100.0D0)
        R4T=REAL(WVPERI)
        R4H=REAL(WVHEIG*100.0D0)
        N=3
        CALL VF_CND0(N,R4D,R4T,R4H,R4W1,R4E1,R4K,R4L,R4C,R4CC)
        WVLENG=DBLE(R4L)*0.01D0
      ELSEIF (IWVTYP.GE. 1) THEN
        R4D=REAL(WVDPTH)
        R4T=REAL(WVPERI)
        R4H=REAL(WVHEIG)
        R4U=0.0
        N  =IWVTYP
        ICN=0
        MSG=0
        CALL vf_sfmb02C(N,R4D,R4T,R4H,R4U,R4A,
     &                  R4L,R4K,R4HH,ICN,R4E1,MSG)
        WVLENG=DBLE(R4L)
        IF (ICN.NE.0)
     &      CALL VF_A2ERR('VF_CSETUP','(VF_SFMB02C) NOT CONVERGED.')
      ELSEIF (IWVTYP.EQ.-3) THEN
        WVLENG=0.0D0
      ELSEIF (IWVTYP.EQ.-4) THEN
        WVLENG=0.0D0
      ELSE
        CALL VF_A2ERR('VF_CWMAK0','P.G ERROR.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
