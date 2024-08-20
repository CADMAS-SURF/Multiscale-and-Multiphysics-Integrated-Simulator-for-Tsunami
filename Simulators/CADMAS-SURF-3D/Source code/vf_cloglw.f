      SUBROUTINE VF_CLOGLW(DL,VA,VT)

CD=== 概要 ===========================================================

CDT   VF_CLOGLW:対数則を満たす摩擦速度の計算
CD      (1)F (x)=vt/κ*log(dl*vt/ν)+vt*A-Va
CD      (2)F'(x)=1/κ*log(dl*vt/ν)+1/κ+A

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    DL : IN  : R*8 : 流速の定義位置から壁面までの距離(△x/2)
CD    VA : IN  : R*8 : 接線方向流速の絶対値(VA>=ZERO)
CD    VT : OUT : R*8 : 摩擦速度

C==== 実行 ===========================================================

CD    -- 流速や粘性係数がゼロならば摩擦速度もゼロとする --
      VT=0.0D0
      IF (VA.LT.ZERO .OR. ANU0.LT.ZERO) GOTO 9000

CD    -- 初期設定 --
C     * Newton法の最大反復回数
      NI=20
C     * 収束判定のための微小量
      EPS=1.0D-10
C     * 各種逆数
      AKR=1.0D0/AKK0
      ANR=1.0D0/ANU0
C     * Newton法の初期値:logの値がゼロになる
      XX=ANU0/DL

CD    -- Newton法の反復 --
      DO 100 I=1,NI
        W=LOG(DL*XX*ANR)
        F=XX*AKR*W+XX*AKA0-VA
        D=AKR*W+AKR+AKA0
        DX=-F/D
        XX=XX+DX
        IF (ABS(DX).LT.XX*EPS) GOTO 110
 100  CONTINUE
 110  CONTINUE
      IF (ABS(DX).GE.XX*EPS) CALL VF_A2ERR('VF_CLOGLW','NOT CONVERGED.')
      VT=XX

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
