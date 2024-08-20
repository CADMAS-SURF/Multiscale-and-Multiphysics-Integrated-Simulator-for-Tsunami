      SUBROUTINE VF_PCDC2P(XX,YY,ZZ,UU,VV,WW,FF,
     &                     BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PCDC2P:マルチグリッド環境の子の情報を親へ転送する

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

CD    -- 子の値を親に転送する --
      IF (MGPRNK.GE.0) THEN
        DO 290 LL=1,4
          IS=1
          JS=1
          KS=1
          IE=MGPINF(1)
          JE=MGPINF(2)
          KE=MGPINF(3)
          IF (MGPINF(4).EQ.0) IS=IS+1
          IF (MGPINF(5).EQ.0) JS=JS+1
          IF (MGPINF(7).EQ.0) IE=IE-1
          IF (MGPINF(8).EQ.0) JE=JE-1
          NN=0
          IF (MGPINF(4).EQ.0) THEN
            DO 130 KK=KS,KE
              DO 120 JJ=JS,JE
                NN=NN+1
                VAL=0.0D0
                K0=1+KD*(KK-1)
                J0=1+JD*(JJ-1)
                I0=2+ID*(IS-1)
                DO 110 KL=1,KD
                  K=K0+KL
                  DO 100 JL=1,JD
                    J=J0+JL
                    IF (LL.EQ.1) THEN
                      VAL=VAL+0.5D0*(FF(I0-1,J,K)+FF(I0,J,K))
                    ENDIF
                    IF (LL.EQ.2) THEN
                      VAL=VAL+UU(I0,J,K)
                    ENDIF
                    IF (LL.EQ.3) THEN
                      VAL=VAL+0.25D0*(VV(I0-1,J  ,K)+VV(I0,J  ,K)+
     &                                VV(I0-1,J+1,K)+VV(I0,J+1,K))
                    ENDIF
                    IF (LL.EQ.4) THEN
                      VAL=VAL+0.25D0*(WW(I0-1,J,K  )+WW(I0,J,K  )+
     &                                WW(I0-1,J,K+1)+WW(I0,J,K+1))
                    ENDIF
 100              CONTINUE
 110            CONTINUE
                DBUF(NN)=VAL/DBLE(JD*KD)
 120          CONTINUE
 130        CONTINUE
          ENDIF
          IF (MGPINF(7).EQ.0) THEN
            DO 180 KK=KS,KE
              DO 170 JJ=JS,JE
                NN=NN+1
                VAL=0.0D0
                K0=1+KD*(KK-1)
                J0=1+JD*(JJ-1)
                I0=  ID*(IE+1)
                DO 160 KL=1,KD
                  K=K0+KL
                  DO 150 JL=1,JD
                    J=J0+JL
                    IF (LL.EQ.1) THEN
                      VAL=VAL+0.5D0*(FF(I0-1,J,K)+FF(I0,J,K))
                    ENDIF
                    IF (LL.EQ.2) THEN
                      VAL=VAL+UU(I0,J,K)
                    ENDIF
                    IF (LL.EQ.3) THEN
                      VAL=VAL+0.25D0*(VV(I0-1,J  ,K)+VV(I0,J  ,K)+
     &                                VV(I0-1,J+1,K)+VV(I0,J+1,K))
                    ENDIF
                    IF (LL.EQ.4) THEN
                      VAL=VAL+0.25D0*(WW(I0-1,J,K  )+WW(I0,J,K  )+
     &                                WW(I0-1,J,K+1)+WW(I0,J,K+1))
                    ENDIF
 150              CONTINUE
 160            CONTINUE
                DBUF(NN)=VAL/DBLE(JD*KD)
 170          CONTINUE
 180        CONTINUE
          ENDIF
          IF (MGPINF(5).EQ.0) THEN
            DO 230 KK=KS,KE
              DO 220 II=IS,IE
                NN=NN+1
                VAL=0.0D0
                K0=1+KD*(KK-1)
                J0=2+JD*(JS-1)
                I0=1+ID*(II-1)
                DO 210 KL=1,KD
                  K=K0+KL
                  DO 200 IL=1,ID
                    I=I0+IL
                    IF (LL.EQ.1) THEN
                      VAL=VAL+0.5D0*(FF(I,J0-1,K)+FF(I,J0,K))
                    ENDIF
                    IF (LL.EQ.2) THEN
                      VAL=VAL+0.25D0*(UU(I  ,J0-1,K)+UU(I  ,J0,K)+
     &                                UU(I+1,J0-1,K)+UU(I+1,J0,K))
                    ENDIF
                    IF (LL.EQ.3) THEN
                      VAL=VAL+VV(I,J0,K)
                    ENDIF
                    IF (LL.EQ.4) THEN
                      VAL=VAL+0.25D0*(WW(I,J0-1,K  )+WW(I,J0,K  )+
     &                                WW(I,J0-1,K+1)+WW(I,J0,K+1))
                    ENDIF
 200              CONTINUE
 210            CONTINUE
                DBUF(NN)=VAL/DBLE(ID*KD)
 220          CONTINUE
 230        CONTINUE
          ENDIF
          IF (MGPINF(8).EQ.0) THEN
            DO 280 KK=KS,KE
              DO 270 II=IS,IE
                NN=NN+1
                VAL=0.0D0
                K0=1+KD*(KK-1)
                J0=  JD*(JE+1)
                I0=1+ID*(II-1)
                DO 260 KL=1,KD
                  K=K0+KL
                  DO 250 IL=1,ID
                    I=I0+IL
                    IF (LL.EQ.1) THEN
                      VAL=VAL+0.5D0*(FF(I,J0-1,K)+FF(I,J0,K))
                    ENDIF
                    IF (LL.EQ.2) THEN
                      VAL=VAL+0.25D0*(UU(I  ,J0-1,K)+UU(I  ,J0,K)+
     &                                UU(I+1,J0-1,K)+UU(I+1,J0,K))
                    ENDIF
                    IF (LL.EQ.3) THEN
                      VAL=VAL+VV(I,J0,K)
                    ENDIF
                    IF (LL.EQ.4) THEN
                      VAL=VAL+0.25D0*(WW(I,J0-1,K  )+WW(I,J0,K  )+
     &                                WW(I,J0-1,K+1)+WW(I,J0,K+1))
                    ENDIF
 250              CONTINUE
 260            CONTINUE
                DBUF(NN)=VAL/DBLE(ID*KD)
 270          CONTINUE
 280        CONTINUE
          ENDIF
          CALL VF_ZXCD_ISENDD(DBUF,NN,MGPRNK,IREQ,IERR)
          CALL VF_ZXCD_WAIT(IREQ,IERR)
 290    CONTINUE
      ENDIF

CD    -- 子の値を親が受信する --
      DO 600 IC=1,MGCNUM
        DO 590 LL=1,4
          IS=MGCPOS(1,IC)
          JS=MGCPOS(2,IC)
          KS=MGCPOS(3,IC)
          IE=MGCPOS(4,IC)
          JE=MGCPOS(5,IC)
          KE=MGCPOS(6,IC)
          IF (MGCINF(4,IC).EQ.0) IS=IS+1
          IF (MGCINF(5,IC).EQ.0) JS=JS+1
          IF (MGCINF(7,IC).EQ.0) IE=IE-1
          IF (MGCINF(8,IC).EQ.0) JE=JE-1
          NN=0
          IF (MGCINF(4,IC).EQ.0) NN=NN+(JE-JS+1)*(KE-KS+1)
          IF (MGCINF(7,IC).EQ.0) NN=NN+(JE-JS+1)*(KE-KS+1)
          IF (MGCINF(5,IC).EQ.0) NN=NN+(IE-IS+1)*(KE-KS+1)
          IF (MGCINF(8,IC).EQ.0) NN=NN+(IE-IS+1)*(KE-KS+1)
          CALL VF_ZXCD_IRECVD(DBUF,NN,MGCRNK(IC),IREQ,IERR)
          CALL VF_ZXCD_WAIT(IREQ,IERR)
          NN=0
          IF (MGCINF(4,IC).EQ.0) THEN
            DO 310 K=KS,KE
              DO 300 J=JS,JE
                NN=NN+1
                VAL=DBUF(NN)
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
 300          CONTINUE
 310        CONTINUE
          ENDIF
          IF (MGCINF(7,IC).EQ.0) THEN
            DO 360 K=KS,KE
              DO 350 J=JS,JE
                NN=NN+1
                VAL=DBUF(NN)
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
 350          CONTINUE
 360        CONTINUE
          ENDIF
          IF (MGCINF(5,IC).EQ.0) THEN
            DO 410 K=KS,KE
              DO 400 J=IS,IE
                NN=NN+1
                VAL=DBUF(NN)
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
 400          CONTINUE
 410        CONTINUE
          ENDIF
          IF (MGCINF(8,IC).EQ.0) THEN
            DO 460 K=KS,KE
              DO 450 J=IS,IE
                NN=NN+1
                VAL=DBUF(NN)
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
 450          CONTINUE
 460        CONTINUE
          ENDIF
 590    CONTINUE
 600  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
