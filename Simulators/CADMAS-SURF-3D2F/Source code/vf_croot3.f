      SUBROUTINE VF_CROOT3(AA,BB,CC,DD,XX1,XX2,XX3,NRT)

CD=== 概要 ===========================================================

CDT   VF_CROOT3:3次以下の方程式の根を計算する(CARDANOの方法)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'

CD    -- 引数 --
CD    AA  : IN  : R*8 : 係数(AA*X3+BB*X2+CC*X+DD=0)
CD    BB  : IN  : R*8 : 係数(AA*X3+BB*X2+CC*X+DD=0)
CD    CC  : IN  : R*8 : 係数(AA*X3+BB*X2+CC*X+DD=0)
CD    DD  : IN  : R*8 : 係数(AA*X3+BB*X2+CC*X+DD=0)
CD    XX1 : OUT : R*8 : 根1
CD    XX2 : OUT : R*8 : 根2
CD    XX3 : OUT : R*8 : 根3
CD    NRT : OUT : R*8 : 根の数

C==== 実行 ===========================================================

CD    -- 2次以下の方程式ならば --
      IF (AA.EQ.0.0D0) THEN
        CALL VF_CROOT2(BB,CC,DD,XX1,XX2,NRT)
        XX3=0.0D0

CD    -- 3次方程式ならば --
      ELSE
C       * X3+AX2+BX+C=0に変形
        C=1.0D0/AA
        A=BB*C
        B=CC*C
        C=DD*C
C       * Y3+3PY+Q=0に変形
        P=B/3.0D0-A*A/9.0D0
        Q=C-A*B/3.0D0+2.0D0*A*A*A/27.0D0
        D=Q*Q+4.0D0*P*P*P
C       * 実根
        IF (D.GT.0.0D0) THEN
          IF (Q.LE.0.0D0) THEN
            R1=0.5D0*(-Q+SQRT(D))
          ELSE
            R1=0.5D0*(-Q-SQRT(D))
          ENDIF
          R2=-P*P*P/R1
          R13=(ABS(R1))**(1.0D0/3.0D0)
          IF (R1.LT.0.0D0) R13=-R13
          R23=(ABS(R2))**(1.0D0/3.0D0)
          IF (R2.LT.0.0D0) R23=-R23
          XX1=R13+R23-A/3.0D0
          XX2=0.0D0
          XX3=0.0D0
          NRT=1
C       * 3実根
        ELSE
          IF (Q.NE.0.0D0) THEN
            T=ATAN(SQRT(-D)/(-Q))
          ELSE
            T=PI/2.0D0
          ENDIF
          IF (Q.GT.0.0D0) T=PI+T
          R=2.0D0*SQRT(-P)
          S=-A/3.0D0
          XX1= R*COS(    T /3.0D0)+S
          XX2=-R*COS((PI-T)/3.0D0)+S
          XX3=-R*COS((PI+T)/3.0D0)+S
          NRT=3
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
