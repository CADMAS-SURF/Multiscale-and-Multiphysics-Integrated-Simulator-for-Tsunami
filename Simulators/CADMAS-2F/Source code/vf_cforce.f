      SUBROUTINE VF_CFORCE(XX,YY,ZZ,PP,FF,NF,ISW,VAL,
     &                     I1IN,J1IN,K1IN,I2IN,J2IN,K2IN)

CD=== 概要 ===========================================================

CDT   VF_CFORCE:指定範囲内の流体と構造物間の波力を計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    PP(@FOR-3D@)   : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    ISW            : IN  : I*4 : 波力の方向
CD                                  =1:障害物へのxの負方向の波力
CD                                  =2:障害物へのxの正方向の波力
CD                                  =3:障害物へのyの負方向の波力
CD                                  =4:障害物へのyの正方向の波力
CD                                  =5:障害物へのzの負方向の波力
CD                                  =6:障害物へのzの正方向の波力
CD    VAL            : OUT : R*8 : 波力
CD    I1IN           : IN  : I*4 : 始点のx方向セル番号
CD    J1IN           : IN  : I*4 : 始点のy方向セル番号
CD    K1IN           : IN  : I*4 : 始点のz方向セル番号
CD    I2IN           : IN  : I*4 : 終点のx方向セル番号
CD    J2IN           : IN  : I*4 : 終点のy方向セル番号
CD    K2IN           : IN  : I*4 : 終点のz方向セル番号
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION PP(NUMI,NUMJ,NUMK),FF(NUMI,NUMJ,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分等 --
      I1=I1IN-(MYGIS-1)
      I2=I2IN-(MYGIS-1)
      J1=J1IN-(MYGJS-1)
      J2=J2IN-(MYGJS-1)
      K1=K1IN
      K2=K2IN
      I1=MAX(I1,MYIS)
      I2=MIN(I2,MYIE)
      J1=MAX(J1,MYJS)
      J2=MIN(J2,MYJE)

CD    -- x方向 --
      IF     (ISW.EQ.1 .OR. ISW.EQ.2) THEN
        IF (MYMIS.EQ.1) THEN
          L1=I1+1
        ELSE
          L1=I1IN-(MYGIS-1)
          IF (L1.LT.MYIS) L1=MYIS-1
        ENDIF
        L2=I2
        FM=0.0D0
        FP=0.0D0
        DO 120 K=K1,K2
          DO 110 J=J1,J2
            DO 100 I=L1,L2
              N1=NF(I-1,J,K)
              N2=NF(I  ,J,K)
              IF     (N1.EQ.-1) THEN
                IF     (N2.EQ.0) THEN
                  DS=YY(2,J)*ZZ(2,K)
                  P =PP(I,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.3) THEN
                  DL=FF(I,J,K)*YY(2,J)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+YY(2,J-1)/2.0D0)*PP(I,J-1,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.4) THEN
                  DL=FF(I,J,K)*YY(2,J)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+YY(2,J+1)/2.0D0)*PP(I,J+1,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.5) THEN
                  DL=FF(I,J,K)*ZZ(2,K)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+ZZ(2,K-1)/2.0D0)*PP(I,J,K-1)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.6) THEN
                  DL=FF(I,J,K)*ZZ(2,K)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+ZZ(2,K+1)/2.0D0)*PP(I,J,K+1)
                  FM=FM-DS*P
                ENDIF
              ELSEIF (N2.EQ.-1) THEN
                IF     (N1.EQ.0) THEN
                  DS=YY(2,J)*ZZ(2,K)
                  P =PP(I-1,J,K)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.3) THEN
                  DL=FF(I-1,J,K)*YY(2,J)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+YY(2,J-1)/2.0D0)*PP(I-1,J-1,K)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.4) THEN
                  DL=FF(I-1,J,K)*YY(2,J)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+YY(2,J+1)/2.0D0)*PP(I-1,J+1,K)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.5) THEN
                  DL=FF(I-1,J,K)*ZZ(2,K)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+ZZ(2,K-1)/2.0D0)*PP(I-1,J,K-1)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.6) THEN
                  DL=FF(I-1,J,K)*ZZ(2,K)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+ZZ(2,K+1)/2.0D0)*PP(I-1,J,K+1)
                  FP=FP+DS*P
                ENDIF
              ENDIF
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE
        VAL=FM
        IF (ISW.EQ.2) VAL=FP

CD    -- y方向 --
      ELSEIF (ISW.EQ.3 .OR. ISW.EQ.4) THEN
        IF (MYMJS.EQ.1) THEN
          L1=J1+1
        ELSE
          L1=J1IN-(MYGJS-1)
          IF (L1.LT.MYJS) L1=MYJS-1
        ENDIF
        L2=J2
        FM=0.0D0
        FP=0.0D0
        DO 220 K=K1,K2
          DO 210 J=L1,L2
            DO 200 I=I1,I2
              N1=NF(I,J-1,K)
              N2=NF(I,J  ,K)
              IF     (N1.EQ.-1) THEN
                IF     (N2.EQ.0) THEN
                  DS=XX(2,I)*ZZ(2,K)
                  P =PP(I,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.1) THEN
                  DL=FF(I,J,K)*XX(2,I)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+XX(2,I-1)/2.0D0)*PP(I-1,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.2) THEN
                  DL=FF(I,J,K)*XX(2,I)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+XX(2,I+1)/2.0D0)*PP(I+1,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.5) THEN
                  DL=FF(I,J,K)*ZZ(2,K)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+ZZ(2,K-1)/2.0D0)*PP(I,J,K-1)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.6) THEN
                  DL=FF(I,J,K)*ZZ(2,K)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+ZZ(2,K+1)/2.0D0)*PP(I,J,K+1)
                  FM=FM-DS*P
                ENDIF
              ELSEIF (N2.EQ.-1) THEN
                IF     (N1.EQ.0) THEN
                  DS=XX(2,I)*ZZ(2,K)
                  P =PP(I,J-1,K)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.1) THEN
                  DL=FF(I,J-1,K)*XX(2,I)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+XX(2,I-1)/2.0D0)*PP(I-1,J-1,K)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.2) THEN
                  DL=FF(I,J-1,K)*XX(2,I)
                  DS=DL*ZZ(2,K)
                  P =DL/2.0D0/(DL+XX(2,I+1)/2.0D0)*PP(I+1,J-1,K)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.5) THEN
                  DL=FF(I,J-1,K)*ZZ(2,K)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+ZZ(2,K-1)/2.0D0)*PP(I,J-1,K-1)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.6) THEN
                  DL=FF(I,J-1,K)*ZZ(2,K)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+ZZ(2,K+1)/2.0D0)*PP(I,J-1,K+1)
                  FP=FP+DS*P
                ENDIF
              ENDIF
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
        VAL=FM
        IF (ISW.EQ.4) VAL=FP

CD    -- z方向 --
      ELSE
        FM=0.0D0
        FP=0.0D0
        DO 320 K=K1+1,K2
          DO 310 J=J1,J2
            DO 300 I=I1,I2
              N1=NF(I,J,K-1)
              N2=NF(I,J,K  )
              IF     (N1.EQ.-1) THEN
                IF     (N2.EQ.0) THEN
                  DS=XX(2,I)*YY(2,J)
                  P =PP(I,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.1) THEN
                  DL=FF(I,J,K)*XX(2,I)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+XX(2,I-1)/2.0D0)*PP(I-1,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.2) THEN
                  DL=FF(I,J,K)*XX(2,I)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+XX(2,I+1)/2.0D0)*PP(I+1,J,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.3) THEN
                  DL=FF(I,J,K)*YY(2,J)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+YY(2,J-1)/2.0D0)*PP(I,J-1,K)
                  FM=FM-DS*P
                ELSEIF (N2.EQ.4) THEN
                  DL=FF(I,J,K)*YY(2,J)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+YY(2,J+1)/2.0D0)*PP(I,J+1,K)
                  FM=FM-DS*P
                ENDIF
              ELSEIF (N2.EQ.-1) THEN
                IF     (N1.EQ.0) THEN
                  DS=XX(2,I)*YY(2,J)
                  P =PP(I,J,K-1)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.1) THEN
                  DL=FF(I,J,K-1)*XX(2,I)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+XX(2,I-1)/2.0D0)*PP(I-1,J,K-1)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.2) THEN
                  DL=FF(I,J,K-1)*XX(2,I)
                  DS=DL*YY(2,J)
                  P =DL/2.0D0/(DL+XX(2,I+1)/2.0D0)*PP(I+1,J,K-1)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.3) THEN
                  DL=FF(I,J,K-1)*YY(2,J)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+YY(2,J-1)/2.0D0)*PP(I,J-1,K-1)
                  FP=FP+DS*P
                ELSEIF (N1.EQ.4) THEN
                  DL=FF(I,J,K-1)*YY(2,J)
                  DS=DL*XX(2,I)
                  P =DL/2.0D0/(DL+YY(2,J+1)/2.0D0)*PP(I,J+1,K-1)
                  FP=FP+DS*P
                ENDIF
              ENDIF
 300        CONTINUE
 310      CONTINUE
 320    CONTINUE
        VAL=FM
        IF (ISW.EQ.6) VAL=FP

      ENDIF

      W=VAL
      CALL VF_P1SUMD(W,VAL)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
