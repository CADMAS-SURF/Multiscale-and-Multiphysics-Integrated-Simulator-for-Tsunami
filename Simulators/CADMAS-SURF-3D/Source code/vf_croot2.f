      SUBROUTINE VF_CROOT2(AA,BB,CC,XX1,XX2,NRT)

CD=== 概要 ===========================================================

CDT   VF_CROOT2:2次以下の方程式の根を計算する
CD              ABS((X1-X2)*(X1-X2))<1.0D-20は重根とする

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    AA  : IN  : R*8 : 係数(AA*X2+BB*X+CC=0)
CD    BB  : IN  : R*8 : 係数(AA*X2+BB*X+CC=0)
CD    CC  : IN  : R*8 : 係数(AA*X2+BB*X+CC=0)
CD    XX1 : OUT : R*8 : 根1(2根の場合は絶対値が大きい方)
CD    XX2 : OUT : R*8 : 根2
CD    NRT : OUT : R*8 : 根の数

C==== 実行 ===========================================================

CD    -- 1次以下の方程式ならば --
      IF (AA.EQ.0.0D0) THEN
        IF (BB.NE.0.0D0) THEN
          XX1=-CC/BB
          XX2=0.0D0
          NRT=1
        ELSE
          XX1=0.0D0
          XX2=0.0D0
          NRT=0
        ENDIF

CD    -- 2次方程式ならば --
      ELSE
C       * X2+2AX+B=0に変形
        B=1.0D0/AA
        A=0.5D0*BB*B
        B=CC*B
C       * 根の計算
        D=A*A-B
        IF     (ABS(D).LT.1.0D-20) THEN       
          XX1=-A
          XX2=0.0D0
          NRT=1
        ELSEIF (D.LT.0.0D0       ) THEN
          XX1=0.0D0
          XX2=0.0D0
          NRT=0
        ELSEIF (A.GE.0.0D0       ) THEN       
          XX1=-A-SQRT(D)
          XX2=B/XX1
          NRT=2
        ELSE
          XX1=-A+SQRT(D)
          XX2=B/XX1
          NRT=2
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
