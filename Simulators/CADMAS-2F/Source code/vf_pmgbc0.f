      SUBROUTINE VF_PMGBC0(BCU,BCV,BCW,BCP,BCF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PMGBC0:マルチグリッド環境の境界条件を強制設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BCU(NUMB,3)      : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)      : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)      : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)      : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : I/O : I*4 : 境界面のインデックス
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCP(NUMB,3)
      DIMENSION BCF(NUMB)
      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 子との通信部分に設定する --
      DO 300 IC=1,MGCNUM
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
        IF (MGCINF(4,IC).EQ.0) THEN
          DO 110 K=KS,KE
            DO 100 J=JS,JE
              IF (INDX(IS,J,K).GE.1) THEN
                L=INDX(IS,J,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 100        CONTINUE
 110      CONTINUE
        ENDIF
        IF (MGCINF(7,IC).EQ.0) THEN
          DO 160 K=KS,KE
            DO 150 J=JS,JE
              IF (INDX(IE+1,J,K).GE.1) THEN
                L=INDX(IE+1,J,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 150        CONTINUE
 160      CONTINUE
        ENDIF
        IF (MGCINF(5,IC).EQ.0) THEN
          DO 210 K=KS,KE
            DO 200 I=IS,IE
              IF (INDY(I,JS,K).GE.1) THEN
                L=INDY(I,JS,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 200        CONTINUE
 210      CONTINUE
        ENDIF
        IF (MGCINF(8,IC).EQ.0) THEN
          DO 260 K=KS,KE
            DO 250 I=IS,IE
              IF (INDY(I,JE+1,K).GE.1) THEN
                L=INDY(I,JE+1,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 250        CONTINUE
 260      CONTINUE
        ENDIF
 300  CONTINUE

CD    -- 親との通信部分に設定する --
      IF (MGPRNK.GE.0) THEN
        IS=2
        JS=2
        IE=NUMI-1
        JE=NUMJ-1
        IF (MGPINF(4).EQ.0) THEN
          DO 410 K=2,NUMK-1
            DO 400 J=JS,JE
              IF (INDX(IS,J,K).GE.1) THEN
                L=INDX(IS,J,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 400        CONTINUE
 410      CONTINUE
        ENDIF
        IF (MGPINF(7).EQ.0) THEN
          DO 460 K=2,NUMK-1
            DO 450 J=JS,JE
              IF (INDX(IE+1,J,K).GE.1) THEN
                L=INDX(IE+1,J,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 450        CONTINUE
 460      CONTINUE
        ENDIF
        IF (MGPINF(5).EQ.0) THEN
          DO 510 K=2,NUMK-1
            DO 500 I=IS,IE
              IF (INDY(I,JS,K).GE.1) THEN
                L=INDY(I,JS,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 500        CONTINUE
 510      CONTINUE
        ENDIF
        IF (MGPINF(8).EQ.0) THEN
          DO 560 K=2,NUMK-1
            DO 550 I=IS,IE
              IF (INDY(I,JE+1,K).GE.1) THEN
                L=INDY(I,JE+1,K)
                INDB(3,L)=3
                INDB(4,L)=1
                INDB(5,L)=3
                BCU(L,:)=0.0D0
                BCV(L,:)=0.0D0
                BCW(L,:)=0.0D0
                BCP(L,:)=0.0D0
                BCF(L)=0.0D0
              ENDIF
 550        CONTINUE
 560      CONTINUE
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
