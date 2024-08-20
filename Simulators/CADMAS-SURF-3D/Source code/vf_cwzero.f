      SUBROUTINE VF_CWZERO(WVZERO,IWVTYP)

CD=== 概要 ===========================================================

CDT   VF_CWZERO:水位変動がゼロになるときの無次元位相を求める

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    WVZERO : OUT : R*8 : 水位変動がゼロになるときの無次元位相
CD    IWVTYP : IN  : I*4 : 造波波形の種別
CD                         =-2:Stokes波(第5次近似解)
CD                         =-1:Cnoidal波(第3次近似解)
CD                         > 0:流れ関数法Bとその次数

CD    -- 局所変数 --
      REAL*4 R4TN,R4Z,R4TN1,R4Z1,R4TN2,R4Z2,EPS

C==== 実行 ===========================================================

CD    -- 初期設定 --
C     * 初期区間検索時の分割数
      NB=100
C     * 2分法の最大反復回数
      NI=20
C     * 収束判定のための微小量
      EPS=1.0E-8
      IF (IWVTYP.GT.0) EPS=1.0E-6
C     * 解のデフォルト値(解が求まらないとき)
      WVZERO=0.5D0

CD    -- 初期区間の検索(無次元位相1.0から不方向へ) --
      R4TN1=1.0E0
      WVT=DBLE(R4TN1)
      CALL VF_CWMAK1(WVT,WVZ,IWVTYP)
      R4Z1=REAL(WVZ)
      IF (ABS(R4Z1).LT.EPS) THEN
        WVZERO=DBLE(R4TN1)
        GOTO 9000
      ENDIF
      DO 100 L=1,NB
        R4TN2=1.0E0-0.5E0*REAL(L)/REAL(NB)
        WVT=DBLE(R4TN2)
        CALL VF_CWMAK1(WVT,WVZ,IWVTYP)
        R4Z2=REAL(WVZ)
        IF (R4Z1*R4Z2.LE.0.0E0) GOTO 110
        R4TN1=R4TN2
        R4Z1 =R4Z2
 100  CONTINUE
 110  CONTINUE
      IF (ABS(R4Z2).LT.EPS) THEN
        WVZERO=DBLE(R4TN2)
        GOTO 9000
      ENDIF
      IF (R4TN1.EQ.R4TN2) GOTO 9000

CD    -- 2分法により解を求める --
      DO 200 L=1,NI
        R4TN=(R4TN1+R4TN2)*0.5E0
        WVZERO=DBLE(R4TN)
        WVT=WVZERO
        CALL VF_CWMAK1(WVT,WVZ,IWVTYP)
        R4Z=REAL(WVZ)
        IF (ABS(R4Z).LT.EPS) GOTO 210
        IF (R4Z1*R4Z.GE.0.0E0) THEN
          R4TN1=R4TN
          R4Z1 =R4Z
        ELSE
          R4TN2=R4TN
          R4Z2 =R4Z
        ENDIF
 200  CONTINUE
 210  CONTINUE

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
