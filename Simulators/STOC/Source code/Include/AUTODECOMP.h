C     自動領域分割用ワーク
C
C     NDIVX: X方向の領域分割数
C     NDIVY: Y方向の領域分割数
C
C     IDIV1: 部分領域のX方向の範囲(開始点)
C     JDIV1: 部分領域のY方向の範囲(開始点)
C     IDIV2: 部分領域のX方向の範囲(終了点)
C     JDIV2: 部分領域のY方向の範囲(終了点)
C
C     XDIV1: X座標の範囲(開始点)
C     XDIV2: X座標の範囲(終了点)
C     YDIV1: Y座標の範囲(開始点)
C     YDIV2: Y座標の範囲(終了点)
C
C     XCAD1: CADMAS領域のX座標の範囲(開始点)
C     XCAD2: CADMAS領域のX座標の範囲(終了点)
C     YCAD1: CADMAS領域のY座標の範囲(開始点)
C     YCAD2: CADMAS領域のY座標の範囲(終了点)
C
C     ICRDC: 座標系接続フラグ(=1:自分が球面座標で子が直交座標、=0:それ以外)
C     REGN : ICRDC=1のとき、子領域の範囲(変数REGIONのコピー)
C
      INTEGER:: NDIVX,NDIVY
      INTEGER:: IDIVX(MAXPE),JDIVY(MAXPE)
      INTEGER:: IDIV1(MAXPE),JDIV1(MAXPE),IDIV2(MAXPE),JDIV2(MAXPE)
      REAL(8):: XDIV1(MAXPE),XDIV2(MAXPE),YDIV1(MAXPE),YDIV2(MAXPE)
      REAL(8):: XCAD1,XCAD2,YCAD1,YCAD2
      INTEGER:: ICRDC
      REAL(8):: REGN(4)
C
      COMMON /AUTODECOMP/ XDIV1,XDIV2,YDIV1,YDIV2,
     $                    XCAD1,YCAD1,XCAD2,YCAD2,REGN,
     $                    NDIVX,NDIVY,IDIV1,JDIV1,IDIV2,JDIV2,ICRDC,
     $                    IDIVX,JDIVY
