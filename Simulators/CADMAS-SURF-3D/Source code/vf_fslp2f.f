      SUBROUTINE VF_FSLP2F(SX,SY,DX,DY,AA,FVAL)

CD=== 概要 ===========================================================

CDT   VF_FSLP2F:斜面の一次方程式から体積内の水の割合を計算する(2次元)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    SX   : IN  : R*8 : 斜面の法線ベクトル(x方向)
CD    SY   : IN  : R*8 : 斜面の法線ベクトル(y方向)
CD    DX   : IN  : R*8 : 格子間隔(x方向)
CD    DY   : IN  : R*8 : 格子間隔(y方向)
CD    AA   : IN  : R*8 : 斜面の一次方程式の右辺
CD    FVAL : OUT : R*8 : VOF関数Fの値(体積内の水の割合)

C     -- 局所変数 --
      DIMENSION SS(2),DD(2),SD(2)

C==== 実行 ===========================================================

CD    -- 軸をソートする --
      SS(1)=ABS(SX)
      SS(2)=ABS(SY)
      DD(1)=DX
      DD(2)=DY
      SD(1)=SS(1)*DX
      SD(2)=SS(2)*DY
      IF (SD(1).LE.SD(2)) THEN
        L1=1
        L2=2
      ELSE
        L1=2
        L2=1
      ENDIF

CD    -- 体積内の水の割合を計算する --
      CNS=2.0D0*SS(1)*SS(2)
      XMX=SD(1)+SD(2)
      VMX=DD(1)*DD(2)*CNS
      A =1.0D0
      B =0.0D0
      C =0.0D0
      IF     (AA.LE.0.0D0 ) THEN
        VV=0.0D0
      ELSEIF (AA.GE.XMX   ) THEN
        VV=VMX
      ELSEIF (AA.LE.SD(L1)) THEN
        VV=A*AA*AA
      ELSE
        Q =SD(L1)
        A =A-1.0D0
        B =B+2.0D0*Q
        C =C-Q*Q
        IF (AA.LE.SD(L2)) THEN
          VV=A*AA*AA+B*AA+C
        ELSE
          Q =SD(L2)
          A =A-1.0D0
          B =B+2.0D0*Q
          C =C-Q*Q
          VV=A*AA*AA+B*AA+C
        ENDIF
      ENDIF
      FVAL=VV/VMX

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
