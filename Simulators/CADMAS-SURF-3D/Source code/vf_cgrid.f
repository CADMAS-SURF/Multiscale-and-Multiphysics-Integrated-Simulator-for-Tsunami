      SUBROUTINE VF_CGRID(ISW,XYZ,XBUF,NUML)

CD=== 概要 ===========================================================

CDT   VF_CGRID:x,yまたはz方向格子関連データを設定
CD      (1)仮想セルの幅は内側と等しいと仮定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISW             : IN  : I*4 : 方向(X:=1,Y:=2,Z:=3)
CD    XYZ(MAXG1,NUML) : I/O : R*8 : x,yまたはz方向格子座標等
CD    XBUF            : IN  : R*8 : 右端の座標値(通信をしないため)
CD    NUML            : IN  : I*4 : 格子数+1
      DIMENSION XYZ(MAXG1,NUML)

C==== 実行 ===========================================================

CD    -- 初期設定 --
      IF     (ISW.EQ.1) THEN
        NUML0=NUMI0
        LG1=MYGIS
        LG2=MYGIE
      ELSEIF (ISW.EQ.2) THEN
        NUML0=NUMJ0
        LG1=MYGJS
        LG2=MYGJE
      ELSE
        NUML0=NUML
        LG1=1
        LG2=NUML0
      ENDIF
      IF (LG1.EQ.1) LG1=2

CD    -- dx=X(I+1)-X(I)と1/dxの計算 --
      DO 100 I=2,NUML-1
        XYZ(2,I)=XYZ(1,I+1)-XYZ(1,I)
        XYZ(4,I)=1.0D0/XYZ(2,I)
 100  CONTINUE
      IF (LG1.EQ.2) THEN
        XYZ(2,1)=XYZ(2,2)
        XYZ(4,1)=1.0D0/XYZ(2,1)
        XYZ(1,1)=XYZ(1,2)-XYZ(2,1)
      ELSE
        XYZ(2,1)=XYZ(1,2)-XYZ(1,1)
        XYZ(4,1)=1.0D0/XYZ(2,1)
      ENDIF
      IF (LG2.EQ.NUML0) THEN
        XYZ(2,NUML)=XYZ(2,NUML-1)
        XYZ(4,NUML)=1.0D0/XYZ(2,NUML)
      ELSE
        XYZ(2,NUML)=XBUF-XYZ(1,NUML)
        XYZ(4,NUML)=1.0D0/XYZ(2,NUML)
      ENDIF

CD    -- cx=(dx(I)+dx(I-1))/2.0、1/cxと1/(dx+dx)の計算 --
      DO 200 I=2,NUML
        XYZ(3,I)=(XYZ(2,I)+XYZ(2,I-1))/2.0D0
        XYZ(5,I)=1.0D0/XYZ(3,I)
        XYZ(6,I)=1.0D0/(XYZ(2,I)+XYZ(2,I-1))
 200  CONTINUE
      XYZ(3,1)=0.0D0
      XYZ(5,1)=0.0D0
      XYZ(6,1)=0.0D0

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
