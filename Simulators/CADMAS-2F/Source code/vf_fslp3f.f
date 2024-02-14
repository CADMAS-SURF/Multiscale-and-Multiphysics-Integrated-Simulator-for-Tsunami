      SUBROUTINE VF_FSLP3F(SX,SY,SZ,DX,DY,DZ,AA,FVAL)

CD=== 概要 ===========================================================

CDT   VF_FSLP3F:斜面の一次方程式から体積内の水の割合を計算する(3次元)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    SX   : IN  : R*8 : 斜面の法線ベクトル(x方向)
CD    SY   : IN  : R*8 : 斜面の法線ベクトル(y方向)
CD    SZ   : IN  : R*8 : 斜面の法線ベクトル(z方向)
CD    DX   : IN  : R*8 : 格子間隔(x方向)
CD    DY   : IN  : R*8 : 格子間隔(y方向)
CD    DZ   : IN  : R*8 : 格子間隔(z方向)
CD    AA   : IN  : R*8 : 斜面の一次方程式の右辺
CD    FVAL : OUT : R*8 : VOF関数Fの値(体積内の水の割合)

C     -- 局所変数 --
      DIMENSION SS(3),DD(3),SD(3)

C==== 実行 ===========================================================

CD    -- 軸をソートする --
      SS(1)=ABS(SX)
      SS(2)=ABS(SY)
      SS(3)=ABS(SZ)
      DD(1)=DX
      DD(2)=DY
      DD(3)=DZ
      SD(1)=SS(1)*DX
      SD(2)=SS(2)*DY
      SD(3)=SS(3)*DZ
      L1=1
      L2=2
      L3=3
      IF (SD(L1).GT.SD(L2)) THEN
        L =L1
        L1=L2
        L2=L
      ENDIF
      IF (SD(L2).GT.SD(L3)) THEN
        L =L2
        L2=L3
        L3=L
      ENDIF
      IF (SD(L1).GT.SD(L2)) THEN
        L =L1
        L1=L2
        L2=L
      ENDIF

CD    -- 体積内の水の割合を計算する --
      CNS=6.0D0*SS(1)*SS(2)*SS(3)
      XMX=SD(1)+SD(2)+SD(3)
      VMX=DD(1)*DD(2)*DD(3)*CNS
      IF     (AA.LE.0.0D0            ) THEN
        VV=0.0D0
      ELSEIF (AA.GE.XMX              ) THEN
        VV=VMX
      ELSEIF (SD(L1)+SD(L2).LE.SD(L3)) THEN
        A=1.0D0
        B=0.0D0
        C=0.0D0
        D=0.0D0
        Q=SD(L1)
        IF (AA.LE.Q) THEN
          VV=A*AA*AA*AA
        ELSE
          A=A-1.0D0
          B=B+3.0D0*Q
          C=C-3.0D0*Q*Q
          D=D+Q*Q*Q
          Q=SD(L2)
          IF (AA.LE.Q) THEN
            VV=A*AA*AA*AA+B*AA*AA+C*AA+D
          ELSE
            A=A-1.0D0
            B=B+3.0D0*Q
            C=C-3.0D0*Q*Q
            D=D+Q*Q*Q
            Q=SD(L1)+SD(L2)
            IF (AA.LE.Q) THEN
              VV=A*AA*AA*AA+B*AA*AA+C*AA+D
            ELSE
              A=A+1.0D0
              B=B-3.0D0*Q
              C=C+3.0D0*Q*Q
              D=D-Q*Q*Q
              Q=SD(L3)
              IF (AA.LE.Q) THEN
                VV=A*AA*AA*AA+B*AA*AA+C*AA+D
              ELSE
                A=A-1.0D0
                B=B+3.0D0*Q
                C=C-3.0D0*Q*Q
                D=D+Q*Q*Q
                Q=SD(L1)+SD(L3)
                IF (AA.LE.Q) THEN
                  VV=A*AA*AA*AA+B*AA*AA+C*AA+D
                ELSE
                  A=A+1.0D0
                  B=B-3.0D0*Q
                  C=C+3.0D0*Q*Q
                  D=D-Q*Q*Q
                  Q=SD(L2)+SD(L3)
                  IF (AA.LE.Q) THEN
                    VV=A*AA*AA*AA+B*AA*AA+C*AA+D
                  ELSE
                    A=A+1.0D0
                    B=B-3.0D0*Q
                    C=C+3.0D0*Q*Q
                    D=D-Q*Q*Q
                    VV=A*AA*AA*AA+B*AA*AA+C*AA+D
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        A=1.0D0
        B=0.0D0
        C=0.0D0
        D=0.0D0
        Q=SD(L1)
        IF (AA.LE.Q) THEN
          VV=A*AA*AA*AA
        ELSE
          A=A-1.0D0
          B=B+3.0D0*Q
          C=C-3.0D0*Q*Q
          D=D+Q*Q*Q
          Q=SD(L2)
          IF (AA.LE.Q) THEN
            VV=A*AA*AA*AA+B*AA*AA+C*AA+D
          ELSE
            A=A-1.0D0
            B=B+3.0D0*Q
            C=C-3.0D0*Q*Q
            D=D+Q*Q*Q
            Q=SD(L3)
            IF (AA.LE.Q) THEN
              VV=A*AA*AA*AA+B*AA*AA+C*AA+D
            ELSE
              A=A-1.0D0
              B=B+3.0D0*Q
              C=C-3.0D0*Q*Q
              D=D+Q*Q*Q
              Q=SD(L1)+SD(L2)
              IF (AA.LE.Q) THEN
                VV=A*AA*AA*AA+B*AA*AA+C*AA+D
              ELSE
                A=A+1.0D0
                B=B-3.0D0*Q
                C=C+3.0D0*Q*Q
                D=D-Q*Q*Q
                Q=SD(L1)+SD(L3)
                IF (AA.LE.Q) THEN
                  VV=A*AA*AA*AA+B*AA*AA+C*AA+D
                ELSE
                  A=A+1.0D0
                  B=B-3.0D0*Q
                  C=C+3.0D0*Q*Q
                  D=D-Q*Q*Q
                  Q=SD(L2)+SD(L3)
                  IF (AA.LE.Q) THEN
                    VV=A*AA*AA*AA+B*AA*AA+C*AA+D
                  ELSE
                    A=A+1.0D0
                    B=B-3.0D0*Q
                    C=C+3.0D0*Q*Q
                    D=D-Q*Q*Q
                    VV=A*AA*AA*AA+B*AA*AA+C*AA+D
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      FVAL=VV/VMX

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
