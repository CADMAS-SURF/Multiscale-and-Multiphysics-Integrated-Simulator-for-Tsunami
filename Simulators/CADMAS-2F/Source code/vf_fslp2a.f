      SUBROUTINE VF_FSLP2A(SX,SY,DX,DY,FVAL,AA)

CD=== 概要 ===========================================================

CDT   VF_FSLP2A:斜面の一次方程式の右辺を計算する(2次元)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    SX   : IN  : R*8 : 斜面の法線ベクトル(x方向)
CD    SY   : IN  : R*8 : 斜面の法線ベクトル(y方向)
CD    DX   : IN  : R*8 : 格子間隔(x方向)
CD    DY   : IN  : R*8 : 格子間隔(y方向)
CD    FVAL : IN  : R*8 : VOF関数Fの値(体積内の水の割合)
CD    AA   : OUT : R*8 : 斜面の一次方程式の右辺

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

CD    -- 右辺の候補を計算する --
      CNS=2.0D0*SS(1)*SS(2)
      XMX=SD(1)+SD(2)
      VMX=DD(1)*DD(2)*CNS
      VIN=VMX*FVAL
      XL=0.0D0
      XR=SD(L1)
      A =1.0D0
      B =0.0D0
      C =0.0D0
      VV=A*XR*XR
      IF (VIN.LE.VV) THEN
        CX=C-VIN
        CALL VF_CROOT2(A,B,CX,XX1,XX2,NRT)
      ELSE
        XL=XR
        XR=SD(L2)
        Q =SD(L1)
        A =A-1.0D0
        B =B+2.0D0*Q
        C =C-Q*Q
        VV=A*XR*XR+B*XR+C
        IF (VIN.LE.VV) THEN
          CX=C-VIN
          CALL VF_CROOT2(A,B,CX,XX1,XX2,NRT)
        ELSE
          XL=XR
          XR=XMX
          Q =SD(L2)
          A =A-1.0D0
          B =B+2.0D0*Q
          C =C-Q*Q
          VV=VMX
          IF (VIN.LE.VV) THEN
            CX=C-VIN
            CALL VF_CROOT2(A,B,CX,XX1,XX2,NRT)
          ELSE
            CALL VF_A2ERR('VF_FSLP2A','P.G ERROR(1).')
          ENDIF
        ENDIF
      ENDIF

CD    -- 右辺の解を特定する --
      EP=XMX*1.0D-6
      IF     (NRT.EQ.0                       ) THEN
        CALL VF_A2ERR('VF_FSLP2A','P.G ERROR(2).')
      ELSEIF (NRT.EQ.1                       ) THEN
        IF     (XX1.GE.XL-EP .AND. XX1.LE.XR+EP) THEN
          AA=XX1
        ELSE
          WRITE(ILPFIL,9510) 'VF_FSLP2A WARNING(1).'
          AA=(XL+XR)/2.0D0
        ENDIF
      ELSE
        IF     (XX1.GE.XL-EP .AND. XX1.LE.XR+EP) THEN
          AA=XX1
        ELSEIF (XX2.GE.XL-EP .AND. XX2.LE.XR+EP) THEN
          AA=XX2
        ELSE
          WRITE(ILPFIL,9510) 'VF_FSLP2A WARNING(2).'
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
