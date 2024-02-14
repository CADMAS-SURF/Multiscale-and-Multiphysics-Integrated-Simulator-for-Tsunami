      SUBROUTINE VF_CLOGKS(DL,VA,BKS,VT)

CD=== 概要 ===========================================================

CDT   VF_CLOGKS:完全粗面の摩擦速度の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    DL  : IN  : R*8 : 流速の定義位置から壁面までの距離(△x/2)
CD    VA  : IN  : R*8 : 接線方向流速の絶対値(VA>=ZERO)
CD    BKS : IN  : R*8 : 壁面の粗さ
CD    VT  : OUT : R*8 : 摩擦速度

C==== 実行 ===========================================================

CD    -- 流速や粘性係数がゼロならば摩擦速度もゼロとする --
      VT=0.0D0
      IF (VA.LT.ZERO .OR. ANU0.LT.ZERO) GOTO 9000

CD    -- 摩擦速度の計算 --
      XX=LOG(DL/BKS)/AKK0+AKA0+3.0D0
      VT=VA/XX

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
