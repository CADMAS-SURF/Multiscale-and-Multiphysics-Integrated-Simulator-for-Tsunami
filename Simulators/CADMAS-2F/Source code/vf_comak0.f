      SUBROUTINE VF_COMAK0(WVDPTH,WVPERI,PI,GG,WVLENG,IWVTYP)

CD=== 概要 ===========================================================

CDT   VF_COMAK0:微小振幅波の波速の計算(微小振幅波による放射境界用)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    WVDPTH : IN  : R*8 : 水深
CD    WVPERI : IN  : R*8 : 周期
CD    PI     : IN  : R*8 : 円周率
CD    GG     : IN  : R*8 : 重力加速度
CD    WVLENG : OUT : R*8 : 波長
CD    IWVTYP : IN  : I*4 : 開境界の種別
CD                         = 0:放射境界(微小振幅波の波速)

C==== 実行 ===========================================================

CD    -- 初期設定 --
C     * Newton法の最大反復回数
      MI=10
C     * 収束判定のための微小量
      EPS=1.0D-10
C     * エラーチェッカー用のダミー
      IDMY=IWVTYP
      IDMY=IDMY

CD    -- 初期値の計算 --
      D=4.0D0*PI*PI*WVDPTH/GG/WVPERI/WVPERI
      IF (D.GE.1.0D0) THEN
        X=D
      ELSE
        X=SQRT(D)
      ENDIF

CD    -- Newton法による収束計算 --
      DO 10 ITR=1,MI
        EXPX =EXP(X)
        EXPXI=1.0D0/EXPX
        COTHX=(EXPX+EXPXI)/(EXPX-EXPXI)
        DX=(X-D*COTHX)/(1.0D0+D*(COTHX*COTHX-1.0D0))
        X =X-DX
        IF (ABS(DX/X).LE.EPS) GOTO 20
 10   CONTINUE
 20   CONTINUE

CD    -- 波長の計算 --
      WVLENG=2.0D0*PI*WVDPTH/X

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
