      SUBROUTINE USER_EOS(ISTATE,SPARAM,PP,RHOG,DRHODP)

CD=== 概要 ===========================================================

CDT   USER_EOS:状態方程式 rho=rho(p)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    ISTATE    : IN  : I*4 : 状態方程式番号
CD    SPARAM    : IN  : R*8 : 状態方程式のパラメータ
CD    PP        : IN  : R*8 : 圧力 p 
CD    RHOG      : OUT : R*8 : 気相密度 ρ
CD    DRHODP    : OUT : R*8 : 気相密度の圧力による微係数 (dρ/dp)/ρ

      DIMENSION SPARAM(*)
      
C==== 実行 ===========================================================

      IF(ISTATE.EQ.1) THEN
C
C P0    : 基準圧力[Pa]
C RCONS : 気体定数[J/kg/K]
C TCONS : 温度[K]
C
        P0    = SPARAM(1)
        RCONS = SPARAM(2)
        TCONS = SPARAM(3)

        RHOG   = (PP + P0)/(RCONS*TCONS)
        DRHODP = 1.0D0/(PP + P0)

      ELSE
        CALL VF_A2ERR('USER_EOS','ISTATE ERROR.')
      END IF

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
