      SUBROUTINE VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                     BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                     IFLG,INDXYZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &                     IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIBOUN:ある方向の境界条件データ(B.C.)を入力

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)       : I/O : R*8 : 流速の境界条件(壁面の粗さ)
CD    BCK(NUMB)        : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    BCT(NUMB)        : I/O : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : I/O : R*8 : 温度の境界条件
CD    BCC(NUMB,LEQC)    : I/O : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : I/O : R*8 : 濃度の境界条件
CD    IFLG             : IN  : I*4 : 種別フラグ
CD                                   =-1:B.C. D(X)
CD                                   =-2:B.C. D(Y)
CD                                   =-3:B.C. D(Z)
CD                                   = 1:B.C. X
CD                                   = 2:B.C. Y
CD                                   = 3:B.C. Z
CD    INDXYZ(@FOR-3D@) : IN  : I*4 : x,y,z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : I/O : I*4 : 境界面のインデックス
CD    INDBK(NUMB)      : I/O : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : I/O : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : I/O : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : I/O : I*4 : 濃度の境界条件
CD    IS(MAXWDS)       : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS)       : IN  : I*4   : n番目の単語の終了位置
CD    NWD              : IN  : I*4   : 単語の数
CD    TEXT             : IN  : C*(*) : 入力した文字列
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION BCVI(NUMB),BCK(NUMB),BCE(NUMB),BCT(NUMB),BCTI(2,NUMB)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION INDXYZ(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
      DIMENSION INDBK(NUMB),INDBE(NUMB),INDBT(NUMB),INDBC(NUMB,LEQC)
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IPP=MYGIS-1
      JPP=MYGJS-1

CD    -- 種別フラグによる分類 --
      IF     (IFLG.EQ.-1) THEN
        IP=2
        I1=1
        J1=1
        K1=1
        I2=NUMI0-1
        J2=NUMJ0-2
        K2=NUMK -2
      ELSEIF (IFLG.EQ.-2) THEN
        IP=2
        I1=1
        J1=1
        K1=1
        I2=NUMI0-2
        J2=NUMJ0-1
        K2=NUMK -2
      ELSEIF (IFLG.EQ.-3) THEN
        IP=2
        I1=1
        J1=1
        K1=1
        I2=NUMI0-2
        J2=NUMJ0-2
        K2=NUMK -1
      ELSEIF (IFLG.EQ.1) THEN
        IP=8
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOI(J1,TEXT(IS(4):IE(4)))
        CALL VF_ZSTOI(K1,TEXT(IS(5):IE(5)))
        CALL VF_ZSTOI(I2,TEXT(IS(6):IE(6)))
        CALL VF_ZSTOI(J2,TEXT(IS(7):IE(7)))
        CALL VF_ZSTOI(K2,TEXT(IS(8):IE(8)))
        IF (I1.LT.1  .OR. I2.GT.NUMI0-1 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                 CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
      ELSEIF (IFLG.EQ.2) THEN
        IP=8
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOI(J1,TEXT(IS(4):IE(4)))
        CALL VF_ZSTOI(K1,TEXT(IS(5):IE(5)))
        CALL VF_ZSTOI(I2,TEXT(IS(6):IE(6)))
        CALL VF_ZSTOI(J2,TEXT(IS(7):IE(7)))
        CALL VF_ZSTOI(K2,TEXT(IS(8):IE(8)))
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-1 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -2 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                 CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
      ELSEIF (IFLG.EQ.3) THEN
        IP=8
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        CALL VF_ZSTOI(I1,TEXT(IS(3):IE(3)))
        CALL VF_ZSTOI(J1,TEXT(IS(4):IE(4)))
        CALL VF_ZSTOI(K1,TEXT(IS(5):IE(5)))
        CALL VF_ZSTOI(I2,TEXT(IS(6):IE(6)))
        CALL VF_ZSTOI(J2,TEXT(IS(7):IE(7)))
        CALL VF_ZSTOI(K2,TEXT(IS(8):IE(8)))
        IF (I1.LT.1  .OR. I2.GT.NUMI0-2 .OR.
     &      J1.LT.1  .OR. J2.GT.NUMJ0-2 .OR.
     &      K1.LT.1  .OR. K2.GT.NUMK -1 .OR.
     &      I1.GT.I2 .OR. J1.GT.J2 .OR. K1.GT.K2)
     &                 CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
      ELSE
        CALL VF_A2ERR('VF_IIBOUN','P.G ERROR.')
      ENDIF
      I1=I1+1
      J1=J1+1
      K1=K1+1
      I2=I2+1
      J2=J2+1
      K2=K2+1

CD    -- 物理量選択部分にポインタを移動 --
      IP=IP+1
      IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX.')

CD    -- 流速・圧力の境界条件 --
      IF     (TEXT(IS(IP):IE(IP)).EQ.'VP') THEN
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX.')
        IF     (TEXT(IS(IP):IE(IP)).EQ.'SLIP') THEN
          IB=1
          BU=0.0D0
          BV=0.0D0
          BW=0.0D0
          BP=0.0D0
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'NON-S') THEN
          IB=2
          BU=0.0D0
          BV=0.0D0
          BW=0.0D0
          BP=0.0D0
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FIX-V') THEN
          IP=IP+3
          IF (NWD.LT.IP) CALL VF_A2ERR('IIBOUN','SYNTAX ERROR.')
          IB=3
          CALL VF_ZSTOR(BU,TEXT(IS(IP-2):IE(IP-2)))
          CALL VF_ZSTOR(BV,TEXT(IS(IP-1):IE(IP-1)))
          CALL VF_ZSTOR(BW,TEXT(IS(IP  ):IE(IP  )))
          BP=0.0D0
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE') THEN
          IB=4
          BU=0.0D0
          BV=0.0D0
          BW=0.0D0
          BP=0.0D0
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'LOG') THEN
          IB=6
          BU=0.0D0
          BV=0.0D0
          BW=0.0D0
          BP=0.0D0
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'LOG-KS') THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('IIBOUN','SYNTAX ERROR.')
          IB=8
          BU=0.0D0
          BV=0.0D0
          BW=0.0D0
          BP=0.0D0
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          IF (BT.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSE
          CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
        ENDIF
        DO 120 K=K1,K2
          DO 110 J=J1,J2
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 100 I=I1,I2
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  L=INDXYZ(I-IPP,J-JPP,K)
                  IF (L.GE.1) THEN
                    INDB(3,L)=IB
                    BCU (  L)=BU
                    BCV (  L)=BV
                    BCW (  L)=BW
                    BCP (  L)=BP
                    BCVI(  L)=BT
                  ENDIF
                ENDIF
 100          CONTINUE
            ENDIF
 110      CONTINUE
 120    CONTINUE

CD    -- VOF関数Fの境界条件 --
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'F') THEN
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        IF     (TEXT(IS(IP):IE(IP)).EQ.'FIX') THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=1
          CALL VF_ZSTOR(BF,TEXT(IS(IP):IE(IP)))
          IF (BF.LT.0.0D0 .OR. BF.GT.1.0D0)
     &                   CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE') THEN
          IB=2
          BF=0.0D0
        ELSE
          CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
        ENDIF
        DO 220 K=K1,K2
          DO 210 J=J1,J2
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 200 I=I1,I2
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  L=INDXYZ(I-IPP,J-JPP,K)
                  IF (L.GE.1) THEN
                    INDB(4,L)=IB
                    BCF (  L)=BF
                  ENDIF
                ENDIF
 200          CONTINUE
            ENDIF
 210      CONTINUE
 220    CONTINUE

CD    -- 乱流エネルギの境界条件 --
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'K') THEN
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        IF     (TEXT(IS(IP):IE(IP)).EQ.'FIX-A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          IF (BT.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FIX+A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          IF (BT.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE-A' ) THEN
          IB=-2
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE+A' ) THEN
          IB= 2
          BT=0.0D0
        ELSE
          CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
        ENDIF
        IF (LEQK.NE.0) THEN
          DO 320 K=K1,K2
            DO 310 J=J1,J2
              IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                DO 300 I=I1,I2
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    L=INDXYZ(I-IPP,J-JPP,K)
                    IF (L.GE.1) THEN
                      INDBK(L)=IB
                      BCK  (L)=BT
                    ENDIF
                  ENDIF
 300            CONTINUE
              ENDIF
 310        CONTINUE
 320      CONTINUE
        ENDIF

CD    -- 乱流エネルギ散逸の境界条件 --
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'E') THEN
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        IF     (TEXT(IS(IP):IE(IP)).EQ.'FIX-A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          IF (BT.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FIX+A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          IF (BT.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE-A' ) THEN
          IB=-2
          BT=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE+A' ) THEN
          IB= 2
          BT=0.0D0
        ELSE
          CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
        ENDIF
        IF (LEQK.NE.0) THEN
          DO 420 K=K1,K2
            DO 410 J=J1,J2
              IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                DO 400 I=I1,I2
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    L=INDXYZ(I-IPP,J-JPP,K)
                    IF (L.GE.1) THEN
                      INDBE(L)=IB
                      BCE  (L)=BT
                    ENDIF
                  ENDIF
 400            CONTINUE
              ENDIF
 410        CONTINUE
 420      CONTINUE
        ENDIF

CD    -- 温度の境界条件 --
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'T') THEN
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        IF     (TEXT(IS(IP):IE(IP)).EQ.'FIX-A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FIX+A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE-A' ) THEN
          IB=-2
          BT =0.0D0
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE+A' ) THEN
          IB= 2
          BT =0.0D0
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FLUX-A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-3
          BT =0.0D0
          CALL VF_ZSTOR(BH,TEXT(IS(IP):IE(IP)))
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FLUX+A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 3
          BT =0.0D0
          CALL VF_ZSTOR(BH,TEXT(IS(IP):IE(IP)))
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'TRAN-A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP+1) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-4
          BT =0.0D0
          CALL VF_ZSTOR(BH ,TEXT(IS(IP  ):IE(IP  )))
          CALL VF_ZSTOR(BT0,TEXT(IS(IP+1):IE(IP+1)))
          IF (BH.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'TRAN+A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP+1) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 4
          BT =0.0D0
          CALL VF_ZSTOR(BH ,TEXT(IS(IP  ):IE(IP  )))
          CALL VF_ZSTOR(BT0,TEXT(IS(IP+1):IE(IP+1)))
          IF (BH.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSE
          CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
        ENDIF
        IF (LEQT.NE.0) THEN
          DO 520 K=K1,K2
            DO 510 J=J1,J2
              IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                DO 500 I=I1,I2
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    L=INDXYZ(I-IPP,J-JPP,K)
                    IF (L.GE.1) THEN
                      INDBT (L)=IB
                      BCT   (L)=BT
                      BCTI(1,L)=BH
                      BCTI(2,L)=BT0
                    ENDIF
                  ENDIF
 500            CONTINUE
              ENDIF
 510        CONTINUE
 520      CONTINUE
        ENDIF

CD    -- 濃度の境界条件 --
      ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'C') THEN
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        CALL VF_ZSTOI(LC,TEXT(IS(IP):IE(IP)))
        IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                  CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        IP=IP+1
        IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
        IF     (TEXT(IS(IP):IE(IP)).EQ.'FIX-A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FIX+A'  ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 1
          CALL VF_ZSTOR(BT,TEXT(IS(IP):IE(IP)))
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE-A' ) THEN
          IB=-2
          BT =0.0D0
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FREE+A' ) THEN
          IB= 2
          BT =0.0D0
          BH =0.0D0
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FLUX-A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-3
          BT =0.0D0
          CALL VF_ZSTOR(BH,TEXT(IS(IP):IE(IP)))
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'FLUX+A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 3
          BT =0.0D0
          CALL VF_ZSTOR(BH,TEXT(IS(IP):IE(IP)))
          BT0=0.0D0
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'TRAN-A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP+1) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB=-4
          BT =0.0D0
          CALL VF_ZSTOR(BH ,TEXT(IS(IP  ):IE(IP  )))
          CALL VF_ZSTOR(BT0,TEXT(IS(IP+1):IE(IP+1)))
          IF (BH.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSEIF (TEXT(IS(IP):IE(IP)).EQ.'TRAN+A' ) THEN
          IP=IP+1
          IF (NWD.LT.IP+1) CALL VF_A2ERR('VF_IIBOUN','SYNTAX ERROR.')
          IB= 4
          BT =0.0D0
          CALL VF_ZSTOR(BH ,TEXT(IS(IP  ):IE(IP  )))
          CALL VF_ZSTOR(BT0,TEXT(IS(IP+1):IE(IP+1)))
          IF (BH.LT.ZERO) CALL VF_A2ERR('VF_IIBOUN','INVALID VALUE.')
        ELSE
          CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
        ENDIF
        IF (LEQC.GT.0) THEN
          DO 620 K=K1,K2
            DO 610 J=J1,J2
              IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                DO 600 I=I1,I2
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    L=INDXYZ(I-IPP,J-JPP,K)
                    IF (L.GE.1) THEN
                      INDBC (L,LC)=IB
                      BCC   (L,LC)=BT
                      BCCI(1,L,LC)=BH
                      BCCI(2,L,LC)=BT0
                    ENDIF
                  ENDIF
 600            CONTINUE
              ENDIF
 610        CONTINUE
 620      CONTINUE
        ENDIF

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIBOUN','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
