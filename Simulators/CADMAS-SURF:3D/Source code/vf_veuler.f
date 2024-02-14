      SUBROUTINE VF_VEULER(XX,YY,ZZ,UU,VV,WW,GGV,GLV,GLV0,DBUF,
     &                     FLUU,FLUV,FLUW,QU,
     &                     FLVU,FLVV,FLVW,QV,
     &                     FLWU,FLWV,FLWW,QW,
     &                     NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_VEULER:仮流速をEuler法で計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)   : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)   : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)   : I/O : R*8 : z方向流速
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率
CD    GLV(@FOR-3D@)  : IN  : R*8 : =GGV+(1-GGV)*CM
CD    GLV0(@FOR-3D@) : IN  : R*8 : =GGV+(1-GGV)*CM(時間依存用)
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLUU(@FOR-3D@) : IN  : R*8 : x方向流速のx方向フラックス
CD    FLUV(@FOR-3D@) : IN  : R*8 : x方向流速のy方向フラックス
CD    FLUW(@FOR-3D@) : IN  : R*8 : x方向流速のz方向フラックス
CD    QU(@FOR-3D@)   : IN  : R*8 : x方向流速の生成消滅
CD    FLVU(@FOR-3D@) : IN  : R*8 : y方向流速のx方向フラックス
CD    FLVV(@FOR-3D@) : IN  : R*8 : y方向流速のy方向フラックス
CD    FLVW(@FOR-3D@) : IN  : R*8 : y方向流速のz方向フラックス
CD    QV(@FOR-3D@)   : IN  : R*8 : y方向流速の生成消滅
CD    FLWU(@FOR-3D@) : IN  : R*8 : z方向流速のx方向フラックス
CD    FLWV(@FOR-3D@) : IN  : R*8 : z方向流速のy方向フラックス
CD    FLWW(@FOR-3D@) : IN  : R*8 : z方向流速のz方向フラックス
CD    QW(@FOR-3D@)   : IN  : R*8 : z方向流速の生成消滅
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@) : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@) : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@) : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GLV (NUMI,NUMJ,NUMK)
      DIMENSION GLV0(NUMI,NUMJ,NUMK),DBUF(NUMBUF*MAXBUF)
      DIMENSION FLUU(NUMI,NUMJ,NUMK),FLUV(NUMI,NUMJ,NUMK)
      DIMENSION FLUW(NUMI,NUMJ,NUMK),QU  (NUMI,NUMJ,NUMK)
      DIMENSION FLVU(NUMI,NUMJ,NUMK),FLVV(NUMI,NUMJ,NUMK)
      DIMENSION FLVW(NUMI,NUMJ,NUMK),QV  (NUMI,NUMJ,NUMK)
      DIMENSION FLWU(NUMI,NUMJ,NUMK),FLWV(NUMI,NUMJ,NUMK)
      DIMENSION FLWW(NUMI,NUMJ,NUMK),QW  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=IA+1
      IF (MYMJS.EQ.1) JA=JA+1

CD    -- 減衰項のためのパラメータの計算 --
      IPA=0
      IP1=IDAMP(1)
      IF (IP1.GE.0) THEN
        IPA=IPA+1
        D  =SQRT(GRZ0/DAMP(4,1))
        W1 =DAMP(3,1)
        PX1=DBLE(IP1+1)*DAMP(1,1)*D
        PZ1=DBLE(IP1+1)*DAMP(2,1)*D
      ENDIF
      IP2=IDAMP(2)
      IF (IP2.GE.0) THEN
        IPA=IPA+1
        D  =SQRT(GRZ0/DAMP(4,2))
        W2 =DAMP(3,2)
        PX2=DBLE(IP2+1)*DAMP(1,2)*D
        PZ2=DBLE(IP2+1)*DAMP(2,2)*D
      ENDIF
      IP3=IDAMP(3)
      IF (IP3.GE.0) THEN
        IPA=IPA+1
        D  =SQRT(GRZ0/DAMP(4,3))
        W3 =DAMP(3,3)
        PX3=DBLE(IP3+1)*DAMP(1,3)*D
        PZ3=DBLE(IP3+1)*DAMP(2,3)*D
      ENDIF
      IP4=IDAMP(4)
      IF (IP4.GE.0) THEN
        IPA=IPA+1
        D  =SQRT(GRZ0/DAMP(4,4))
        W4 =DAMP(3,4)
        PX4=DBLE(IP4+1)*DAMP(1,4)*D
        PZ4=DBLE(IP4+1)*DAMP(2,4)*D
      ENDIF
      XMIN=GLXMIN
      XMAX=GLXMAX
      YMIN=GLYMIN
      YMAX=GLYMAX

CD    -- x方向仮流速の計算 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=IA,IB
            IF (INDX(I,J,K).EQ.0 .AND. NF(I-1,J,K)*NF(I,J,K).EQ.0) THEN
              GV =XX(6,I)*(XX(2,I)*GLV (I,J,K)+XX(2,I-1)*GLV (I-1,J,K))
              GV0=XX(6,I)*(XX(2,I)*GLV0(I,J,K)+XX(2,I-1)*GLV0(I-1,J,K))
              DU=( XX(5,I)*(FLUU(I,J,K)-FLUU(I-1,J,K))
     &            +YY(4,J)*(FLUV(I,J+1,K)-FLUV(I,J,K))
     &            +ZZ(4,K)*(FLUW(I,J,K+1)-FLUW(I,J,K))+QU(I,J,K))
              UU(I,J,K)=(GV*UU(I,J,K)+DTNOW*DU)/GV0

              IF (UU(I,J,K).GT. VVMAX) UU(I,J,K)= VVMAX
              IF (UU(I,J,K).LT.-VVMAX) UU(I,J,K)=-VVMAX

C             * 減衰項を陰的に計算
              IF (IPA.NE.0) THEN
                X=XX(1,I)
                Y=(YY(1,J)+YY(1,J+1))*0.5D0
                DMAX=0.0D0
                IF (IP1.GE.0) THEN
                  D=X-XMIN
                  IF (D.LE.W1) THEN
                    D=PX1*(1.0D0-D/W1)**IP1
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP2.GE.0) THEN
                  D=XMAX-X
                  IF (D.LE.W2) THEN
                    D=PX2*(1.0D0-D/W2)**IP2
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP3.GE.0) THEN
                  D=Y-YMIN
                  IF (D.LE.W3) THEN
                    D=PX3*(1.0D0-D/W3)**IP3
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP4.GE.0) THEN
                  D=YMAX-Y
                  IF (D.LE.W4) THEN
                    D=PX4*(1.0D0-D/W4)**IP4
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                G=XX(6,I)*(XX(2,I)*GGV(I,J,K)+XX(2,I-1)*GGV(I-1,J,K))
                D=GV0/(GV0+DTNOW*G*DMAX)
                UU(I,J,K)=UU(I,J,K)*D
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- y方向仮流速の計算 --
      DO 220 K=2,NUMK-1
        DO 210 J=JA,JB
          DO 200 I=MYIS,MYIE
            IF (INDY(I,J,K).EQ.0 .AND. NF(I,J-1,K)*NF(I,J,K).EQ.0) THEN
              GV =YY(6,J)*(YY(2,J)*GLV (I,J,K)+YY(2,J-1)*GLV (I,J-1,K))
              GV0=YY(6,J)*(YY(2,J)*GLV0(I,J,K)+YY(2,J-1)*GLV0(I,J-1,K))
              DV=( XX(4,I)*(FLVU(I+1,J,K)-FLVU(I,J,K))
     &            +YY(5,J)*(FLVV(I,J,K)-FLVV(I,J-1,K))
     &            +ZZ(4,K)*(FLVW(I,J,K+1)-FLVW(I,J,K))+QV(I,J,K))
              VV(I,J,K)=(GV*VV(I,J,K)+DTNOW*DV)/GV0

              IF (VV(I,J,K).GT. VVMAX) VV(I,J,K)= VVMAX
              IF (VV(I,J,K).LT.-VVMAX) VV(I,J,K)=-VVMAX

C             * 減衰項を陰的に計算
              IF (IPA.NE.0) THEN
                X=(XX(1,I)+XX(1,I+1))*0.5D0
                Y=YY(1,J)
                DMAX=0.0D0
                IF (IP1.GE.0) THEN
                  D=X-XMIN
                  IF (D.LE.W1) THEN
                    D=PX1*(1.0D0-D/W1)**IP1
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP2.GE.0) THEN
                  D=XMAX-X
                  IF (D.LE.W2) THEN
                    D=PX2*(1.0D0-D/W2)**IP2
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP3.GE.0) THEN
                  D=Y-YMIN
                  IF (D.LE.W3) THEN
                    D=PX3*(1.0D0-D/W3)**IP3
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP4.GE.0) THEN
                  D=YMAX-Y
                  IF (D.LE.W4) THEN
                    D=PX4*(1.0D0-D/W4)**IP4
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                G=YY(6,J)*(YY(2,J)*GGV(I,J,K)+YY(2,J-1)*GGV(I,J-1,K))
                D=GV0/(GV0+DTNOW*G*DMAX)
                VV(I,J,K)=VV(I,J,K)*D
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- z方向仮流速の計算 --
      DO 320 K=3,NUMK-1
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (INDZ(I,J,K).EQ.0 .AND. NF(I,J,K-1)*NF(I,J,K).EQ.0) THEN
              GV =ZZ(6,K)*(ZZ(2,K)*GLV (I,J,K)+ZZ(2,K-1)*GLV (I,J,K-1))
              GV0=ZZ(6,K)*(ZZ(2,K)*GLV0(I,J,K)+ZZ(2,K-1)*GLV0(I,J,K-1))
              DW=( XX(4,I)*(FLWU(I+1,J,K)-FLWU(I,J,K))
     &            +YY(4,J)*(FLWV(I,J+1,K)-FLWV(I,J,K))
     &            +ZZ(5,K)*(FLWW(I,J,K)-FLWW(I,J,K-1))+QW(I,J,K))
              WW(I,J,K)=(GV*WW(I,J,K)+DTNOW*DW)/GV0

              IF (WW(I,J,K).GT. VVMAX) WW(I,J,K)= VVMAX
              IF (WW(I,J,K).LT.-VVMAX) WW(I,J,K)=-VVMAX

C             * 減衰項を陰的に計算
              IF (IPA.NE.0) THEN
                X=(XX(1,I)+XX(1,I+1))*0.5D0
                Y=(YY(1,J)+YY(1,J+1))*0.5D0
                DMAX=0.0D0
                IF (IP1.GE.0) THEN
                  D=X-XMIN
                  IF (D.LE.W1) THEN
                    D=PZ1*(1.0D0-D/W1)**IP1
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP2.GE.0) THEN
                  D=XMAX-X
                  IF (D.LE.W2) THEN
                    D=PZ2*(1.0D0-D/W2)**IP2
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP3.GE.0) THEN
                  D=Y-YMIN
                  IF (D.LE.W3) THEN
                    D=PZ3*(1.0D0-D/W3)**IP3
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                IF (IP4.GE.0) THEN
                  D=YMAX-Y
                  IF (D.LE.W4) THEN
                    D=PZ4*(1.0D0-D/W4)**IP4
                    IF (D.GT.DMAX) DMAX=D
                  ENDIF
                ENDIF
                G=ZZ(6,K)*(ZZ(2,K)*GGV(I,J,K)+ZZ(2,K-1)*GGV(I,J,K-1))
                D=GV0/(GV0+DTNOW*G*DMAX)
                WW(I,J,K)=WW(I,J,K)*D
              ENDIF
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
