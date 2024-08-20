      SUBROUTINE VF_PCDP2C(XX,YY,ZZ,UU,VV,WW,FF,
     &                     BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PCDP2C:マルチグリッド環境の親の情報を子へ転送する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCF(NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    C@ CAKIY ID,JD,KD
      ID=3
      JD=3
      KD=1

CD    -- 親の値を子に転送する --
      DO 300 IC=1,MGCNUM
        DO 290 LL=1,4
          IS=MGCPOS(1,IC)
          JS=MGCPOS(2,IC)
          KS=MGCPOS(3,IC)
          IE=MGCPOS(4,IC)
          JE=MGCPOS(5,IC)
          KE=MGCPOS(6,IC)
          NN=0
          IF (MGCINF(4,IC).EQ.0) THEN
            DO 110 K=KS,KE
              DO 100 J=JS,JE
                NN=NN+1
                IF (LL.EQ.1) THEN
                  DBUF(NN)=0.5D0*(FF(IS-1,J,K)+FF(IS,J,K))
                ENDIF
                IF (LL.EQ.2) THEN
                  DBUF(NN)=UU(IS,J,K)
                ENDIF
                IF (LL.EQ.3) THEN
                  DBUF(NN)=0.25D0*(VV(IS-1,J  ,K)+VV(IS,J  ,K)+
     &                             VV(IS-1,J+1,K)+VV(IS,J+1,K))
                ENDIF
                IF (LL.EQ.4) THEN
                  DBUF(NN)=0.25D0*(WW(IS-1,J,K  )+WW(IS,J,K  )+
     &                             WW(IS-1,J,K+1)+WW(IS,J,K+1))
                ENDIF
 100          CONTINUE
 110        CONTINUE
          ENDIF
          IF (MGCINF(7,IC).EQ.0) THEN
            DO 160 K=KS,KE
              DO 150 J=JS,JE
                NN=NN+1
                IF (LL.EQ.1) THEN
                  DBUF(NN)=0.5D0*(FF(IE,J,K)+FF(IE+1,J,K))
                ENDIF
                IF (LL.EQ.2) THEN
                  DBUF(NN)=UU(IE+1,J,K)
                ENDIF
                IF (LL.EQ.3) THEN
                  DBUF(NN)=0.25D0*(VV(IE,J  ,K)+VV(IE+1,J  ,K)+
     &                             VV(IE,J+1,K)+VV(IE+1,J+1,K))
                ENDIF
                IF (LL.EQ.4) THEN
                  DBUF(NN)=0.25D0*(WW(IE,J,K  )+WW(IE+1,J,K  )+
     &                             WW(IE,J,K+1)+WW(IE+1,J,K+1))
                ENDIF
 150          CONTINUE
 160        CONTINUE
          ENDIF
          IF (MGCINF(5,IC).EQ.0) THEN
            DO 210 K=KS,KE
              DO 200 I=IS,IE
                NN=NN+1
                IF (LL.EQ.1) THEN
                  DBUF(NN)=0.5D0*(FF(I,JS-1,K)+FF(I,JS,K))
                ENDIF
                IF (LL.EQ.2) THEN
                  DBUF(NN)=0.25D0*(UU(I  ,JS-1,K)+UU(I  ,JS,K)+
     &                             UU(I+1,JS-1,K)+UU(I+1,JS,K))
                ENDIF
                IF (LL.EQ.3) THEN
                  DBUF(NN)=VV(I,JS,K)
                ENDIF
                IF (LL.EQ.4) THEN
                  DBUF(NN)=0.25D0*(WW(I,JS-1,K  )+WW(I,JS,K  )+
     &                             WW(I,JS-1,K+1)+WW(I,JS,K+1))
                ENDIF
 200          CONTINUE
 210        CONTINUE
          ENDIF
          IF (MGCINF(8,IC).EQ.0) THEN
            DO 260 K=KS,KE
              DO 250 I=IS,IE
                NN=NN+1
                IF (LL.EQ.1) THEN
                  DBUF(NN)=0.5D0*(FF(I,JE,K)+FF(I,JE+1,K))
                ENDIF
                IF (LL.EQ.2) THEN
                  DBUF(NN)=0.25D0*(UU(I  ,JE,K)+UU(I  ,JE+1,K)+
     &                             UU(I+1,JE,K)+UU(I+1,JE+1,K))
                ENDIF
                IF (LL.EQ.3) THEN
                  DBUF(NN)=VV(I,JE+1,K)
                ENDIF
                IF (LL.EQ.4) THEN
                  DBUF(NN)=0.25D0*(WW(I,JE,K  )+WW(I,JE+1,K  )+
     &                             WW(I,JE,K+1)+WW(I,JE+1,K+1))
                ENDIF
 250          CONTINUE
 260        CONTINUE
          ENDIF
          CALL VF_ZXCD_ISENDD(DBUF,NN,MGCRNK(IC),IREQ,IERR)
          CALL VF_ZXCD_WAIT(IREQ,IERR)
 290    CONTINUE
 300  CONTINUE

CD    -- 親の値を子が受信する --
      IF (MGPRNK.GE.0) THEN
        DO 590 LL=1,4
          IS=2
          JS=2
          IE=NUMI-1
          JE=NUMJ-1
          NN=0
          IF (MGPINF(4).EQ.0) NN=NN+MGPINF(2)*MGPINF(3)
          IF (MGPINF(7).EQ.0) NN=NN+MGPINF(2)*MGPINF(3)
          IF (MGPINF(5).EQ.0) NN=NN+MGPINF(1)*MGPINF(3)
          IF (MGPINF(8).EQ.0) NN=NN+MGPINF(1)*MGPINF(3)
          CALL VF_ZXCD_IRECVD(DBUF,NN,MGPRNK,IREQ,IERR)
          CALL VF_ZXCD_WAIT(IREQ,IERR)
          NN=0
          IF (MGPINF(4).EQ.0) THEN
            DO 430 KK=1,MGPINF(3)
              DO 420 JJ=1,MGPINF(2)
                NN=NN+1
                VAL=DBUF(NN)
                K0=1+KD*(KK-1)
                J0=1+JD*(JJ-1)
                DO 410 KL=1,KD
                  K=K0+KL
                  DO 400 JL=1,JD
                    J=J0+JL
                    IF (INDX(IS,J,K).GE.1) THEN
                      L=INDX(IS,J,K)
                      IF (LL.EQ.1) BCF(L)=VAL
                      IF (LL.EQ.2) THEN 
                        BCU(L)=VAL
                        UU(IS,J,K)=VAL
                      ENDIF
                      IF (LL.EQ.3) BCV(L)=VAL
                      IF (LL.EQ.4) BCW(L)=VAL
                    ENDIF
 400              CONTINUE
 410            CONTINUE
 420          CONTINUE
 430        CONTINUE
          ENDIF
          IF (MGPINF(7).EQ.0) THEN
            DO 480 KK=1,MGPINF(3)
              DO 470 JJ=1,MGPINF(2)
                NN=NN+1
                VAL=DBUF(NN)
                K0=1+KD*(KK-1)
                J0=1+JD*(JJ-1)
                DO 460 KL=1,KD
                  K=K0+KL
                  DO 450 JL=1,JD
                    J=J0+JL
                    IF (INDX(IE+1,J,K).GE.1) THEN
                      L=INDX(IE+1,J,K)
                      IF (LL.EQ.1) BCF(L)=VAL
                      IF (LL.EQ.2) THEN
                        BCU(L)=VAL
                        UU(IE+1,J,K)=VAL
                      ENDIF
                      IF (LL.EQ.3) BCV(L)=VAL
                      IF (LL.EQ.4) BCW(L)=VAL
                    ENDIF
 450              CONTINUE
 460            CONTINUE
 470          CONTINUE
 480        CONTINUE
          ENDIF
          IF (MGPINF(5).EQ.0) THEN
            DO 530 KK=1,MGPINF(3)
              DO 520 II=1,MGPINF(1)
                NN=NN+1
                VAL=DBUF(NN)
                K0=1+KD*(KK-1)
                I0=1+ID*(II-1)
                DO 510 KL=1,KD
                  K=K0+KL
                  DO 500 IL=1,ID
                    I=I0+IL
                    IF (INDY(I,JS,K).GE.1) THEN
                      L=INDY(I,JS,K)
                      IF (LL.EQ.1) BCF(L)=VAL
                      IF (LL.EQ.2) BCU(L)=VAL
                      IF (LL.EQ.3) THEN
                        BCV(L)=VAL
                        VV(I,JS,K)=VAL
                      ENDIF
                      IF (LL.EQ.4) BCW(L)=VAL
                    ENDIF
 500              CONTINUE
 510            CONTINUE
 520          CONTINUE
 530        CONTINUE
          ENDIF
          IF (MGPINF(8).EQ.0) THEN
            DO 580 KK=1,MGPINF(3)
              DO 570 II=1,MGPINF(1)
                NN=NN+1
                VAL=DBUF(NN)
                K0=1+KD*(KK-1)
                I0=1+ID*(II-1)
                DO 560 KL=1,KD
                  K=K0+KL
                  DO 550 IL=1,ID
                    I=I0+IL
                    IF (INDY(I,JE+1,K).GE.1) THEN
                      L=INDY(I,JE+1,K)
                      IF (LL.EQ.1) BCF(L)=VAL
                      IF (LL.EQ.2) BCU(L)=VAL
                      IF (LL.EQ.3) THEN
                        BCV(L)=VAL
                        VV(I,JE+1,K)=VAL
                      ENDIF
                      IF (LL.EQ.4) BCW(L)=VAL
                    ENDIF
 550              CONTINUE
 560            CONTINUE
 570          CONTINUE
 580        CONTINUE
          ENDIF
 590    CONTINUE
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
