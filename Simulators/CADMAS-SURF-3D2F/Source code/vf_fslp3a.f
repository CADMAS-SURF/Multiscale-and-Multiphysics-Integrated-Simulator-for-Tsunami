      SUBROUTINE VF_FSLP3A(SX,SY,SZ,DX,DY,DZ,FVAL,AA)

CD=== 概要 ===========================================================

CDT   VF_FSLP3A:斜面の一次方程式の右辺を計算する(3次元)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    SX   : IN  : R*8 : 斜面の法線ベクトル(x方向)
CD    SY   : IN  : R*8 : 斜面の法線ベクトル(y方向)
CD    SZ   : IN  : R*8 : 斜面の法線ベクトル(z方向)
CD    DX   : IN  : R*8 : 格子間隔(x方向)
CD    DY   : IN  : R*8 : 格子間隔(y方向)
CD    DZ   : IN  : R*8 : 格子間隔(z方向)
CD    FVAL : IN  : R*8 : VOF関数Fの値(体積内の水の割合)
CD    AA   : OUT : R*8 : 斜面の一次方程式の右辺

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

CD    -- 右辺の候補を計算する --
      CNS=6.0D0*SS(1)*SS(2)*SS(3)
      XMX=SD(1)+SD(2)+SD(3)
      VMX=DD(1)*DD(2)*DD(3)*CNS
      VIN=VMX*FVAL
      IF (SD(L1)+SD(L2).LE.SD(L3)) THEN
        A=1.0D0
        B=0.0D0
        C=0.0D0
        D=0.0D0
        XL=0.0D0
        XR=SD(L1)
        VV=A*XR*XR*XR
        IF (VIN.LE.VV) THEN
          DW=D-VIN
          CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
        ELSE
          A=A-1.0D0
          B=B+3.0D0*XR
          C=C-3.0D0*XR*XR
          D=D+XR*XR*XR
          XL=XR
          XR=SD(L2)
          VV=A*XR*XR*XR+B*XR*XR+C*XR+D
          IF (VIN.LE.VV) THEN
            DW=D-VIN
            CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
          ELSE
            A=A-1.0D0
            B=B+3.0D0*XR
            C=C-3.0D0*XR*XR
            D=D+XR*XR*XR
            XL=XR
            XR=SD(L1)+SD(L2)
            VV=A*XR*XR*XR+B*XR*XR+C*XR+D
            IF (VIN.LE.VV) THEN
              DW=D-VIN
              CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
            ELSE
              A=A+1.0D0
              B=B-3.0D0*XR
              C=C+3.0D0*XR*XR
              D=D-XR*XR*XR
              XL=XR
              XR=SD(L3)
              VV=A*XR*XR*XR+B*XR*XR+C*XR+D
              IF (VIN.LE.VV) THEN
                DW=D-VIN
                CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
              ELSE
                A=A-1.0D0
                B=B+3.0D0*XR
                C=C-3.0D0*XR*XR
                D=D+XR*XR*XR
                XL=XR
                XR=SD(L1)+SD(L3)
                VV=A*XR*XR*XR+B*XR*XR+C*XR+D
                IF (VIN.LE.VV) THEN
                  DW=D-VIN
                  CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
                ELSE
                  A=A+1.0D0
                  B=B-3.0D0*XR
                  C=C+3.0D0*XR*XR
                  D=D-XR*XR*XR
                  XL=XR
                  XR=SD(L2)+SD(L3)
                  VV=A*XR*XR*XR+B*XR*XR+C*XR+D
                  IF (VIN.LE.VV) THEN
                    DW=D-VIN
                    CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
                  ELSE
                    A=A+1.0D0
                    B=B-3.0D0*XR
                    C=C+3.0D0*XR*XR
                    D=D-XR*XR*XR
                    XL=XR
                    XR=XMX
                    VV=VMX
                    IF (VIN.LE.VV) THEN
                      DW=D-VIN
                      CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
                    ELSE
                      CALL VF_A2ERR('VF_FSLP3A','P.G ERROR(1).')
                    ENDIF
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
        XL=0.0D0
        XR=SD(L1)
        VV=A*XR*XR*XR
        IF (VIN.LE.VV) THEN
          DW=D-VIN
          CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
        ELSE
          A=A-1.0D0
          B=B+3.0D0*XR
          C=C-3.0D0*XR*XR
          D=D+XR*XR*XR
          XL=XR
          XR=SD(L2)
          VV=A*XR*XR*XR+B*XR*XR+C*XR+D
          IF (VIN.LE.VV) THEN
            DW=D-VIN
            CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
          ELSE
            A=A-1.0D0
            B=B+3.0D0*XR
            C=C-3.0D0*XR*XR
            D=D+XR*XR*XR
            XL=XR
            XR=SD(L3)
            VV=A*XR*XR*XR+B*XR*XR+C*XR+D
            IF (VIN.LE.VV) THEN
              DW=D-VIN
              CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
            ELSE
              A=A-1.0D0
              B=B+3.0D0*XR
              C=C-3.0D0*XR*XR
              D=D+XR*XR*XR
              XL=XR
              XR=SD(L1)+SD(L2)
              VV=A*XR*XR*XR+B*XR*XR+C*XR+D
              IF (VIN.LE.VV) THEN
                DW=D-VIN
                CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
              ELSE
                A=A+1.0D0
                B=B-3.0D0*XR
                C=C+3.0D0*XR*XR
                D=D-XR*XR*XR
                XL=XR
                XR=SD(L1)+SD(L3)
                VV=A*XR*XR*XR+B*XR*XR+C*XR+D
                IF (VIN.LE.VV) THEN
                  DW=D-VIN
                  CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
                ELSE
                  A=A+1.0D0
                  B=B-3.0D0*XR
                  C=C+3.0D0*XR*XR
                  D=D-XR*XR*XR
                  XL=XR
                  XR=SD(L2)+SD(L3)
                  VV=A*XR*XR*XR+B*XR*XR+C*XR+D
                  IF (VIN.LE.VV) THEN
                    DW=D-VIN
                    CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
                  ELSE
                    A=A+1.0D0
                    B=B-3.0D0*XR
                    C=C+3.0D0*XR*XR
                    D=D-XR*XR*XR
                    XL=XR
                    XR=XMX
                    VV=VMX
                    IF (VIN.LE.VV) THEN
                      DW=D-VIN
                      CALL VF_CROOT3(A,B,C,DW,XX1,XX2,XX3,NRT)
                    ELSE
                      CALL VF_A2ERR('VF_FSLP3A','P.G ERROR(2).')
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

CD    -- 右辺の解を特定する --
      EP=XMX*1.0D-6
      IF     (NRT.EQ.0) THEN
        CALL VF_A2ERR('VF_FSLP3A','P.G ERROR(3).')
      ELSEIF (NRT.EQ.1) THEN
        IF     (XX1.GE.XL-EP .AND. XX1.LE.XR+EP) THEN
          AA=XX1
        ELSE
          WRITE(ILPFIL,9510) 'VF_FSLP3A WARNING(1).'
          AA=(XL+XR)/2.0D0
        ENDIF
      ELSEIF (NRT.EQ.2) THEN
        IF     (XX1.GE.XL-EP .AND. XX1.LE.XR+EP) THEN
          AA=XX1
        ELSEIF (XX2.GE.XL-EP .AND. XX2.LE.XR+EP) THEN
          AA=XX2
        ELSE
          WRITE(ILPFIL,9510) 'VF_FSLP3A WARNING(2).'
          AA=(XL+XR)/2.0D0
        ENDIF
      ELSE
        IF     (XX1.GE.XL-EP .AND. XX1.LE.XR+EP) THEN
          AA=XX1
        ELSEIF (XX2.GE.XL-EP .AND. XX2.LE.XR+EP) THEN
          AA=XX2
        ELSEIF (XX3.GE.XL-EP .AND. XX3.LE.XR+EP) THEN
          AA=XX3
        ELSE
          WRITE(ILPFIL,9510) 'VF_FSLP3A WARNING(3).'
          AA=(XL+XR)/2.0D0
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ',A)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
