      SUBROUTINE VF_IIFILE(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIFILE:ファイル出力制御データ(FILE)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')

CD    -- FILE L/Pを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'L/P') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
C       * FILE L/P STEP
        IF     (TEXT(IS(3):IE(3)).EQ.'STEP') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOI(ILPTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOI(ILPTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(ILPTRN(3),TEXT(IS(6):IE(6)))
          ILPTYP=1
          IF (ILPTRN(1).GT.ILPTRN(2) .OR. ILPTRN(3).LE.0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE L/P TIME
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'TIME') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOR(RLPTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(RLPTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(RLPTRN(3),TEXT(IS(6):IE(6)))
          ILPTYP=2
          IF (RLPTRN(1).GT.RLPTRN(2) .OR. RLPTRN(3).LE.0.0D0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE L/P AREA
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'AREA') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          IF     (TEXT(IS(4):IE(4)).EQ.'XY') THEN
            ILPARA(1)=1
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'XZ') THEN
            ILPARA(1)=2
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'YZ') THEN
            ILPARA(1)=3
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'YX') THEN
            ILPARA(1)=4
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'ZX') THEN
            ILPARA(1)=5
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'ZY') THEN
            ILPARA(1)=6
          ELSE
            CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
          ENDIF
          CALL VF_ZSTOI(ILPARA(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(ILPARA(3),TEXT(IS(6):IE(6)))
          ILPARA(2)=ILPARA(2)+1
          ILPARA(3)=ILPARA(3)+1
          IF     (ILPARA(1).EQ.1 .OR. ILPARA(1).EQ.4) THEN
            IF (ILPARA(2).LT.1 .OR. ILPARA(2).GT.NUMK-1)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
            IF (ILPARA(3).LT.1 .OR. ILPARA(3).GT.NUMK)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ELSEIF (ILPARA(1).EQ.2 .OR. ILPARA(1).EQ.5) THEN
            IF (ILPARA(2).LT.1 .OR. ILPARA(2).GT.NUMJ0-1)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
            IF (ILPARA(3).LT.1 .OR. ILPARA(3).GT.NUMJ0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ELSE
            IF (ILPARA(2).LT.1 .OR. ILPARA(2).GT.NUMI0-1)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
            IF (ILPARA(3).LT.1 .OR. ILPARA(3).GT.NUMI0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ENDIF
C       * FILE L/P ON , FILE L/P OFF
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'ON' .OR.
     &          TEXT(IS(3):IE(3)).EQ.'OFF'    ) THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          ISW=0
          IF (TEXT(IS(3):IE(3)).EQ.'ON') ISW=1
          IF     (TEXT(IS(4):IE(4)).EQ.'OBST'  ) THEN
            ILPON( 1)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'BC-IND') THEN
            ILPON( 2)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'CM0'   ) THEN
            ILPON( 3)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'CD0'   ) THEN
            ILPON( 4)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'POROUS') THEN
            ILPON( 5)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'LAMBDA') THEN
            ILPON( 6)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'NF'    ) THEN
            ILPON( 7)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'F'     ) THEN
            ILPON( 8)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'V'     ) THEN
            ILPON( 9)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'P'     ) THEN
            ILPON(10)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'VISC'  ) THEN
            ILPON(11)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'BC'    ) THEN
            ILPON(12)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'T'     ) THEN
            ILPON(13)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'T-COND') THEN
            ILPON(14)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'C'     ) THEN
            ILPON(15)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'DIFFUS') THEN
            ILPON(16)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'K'     ) THEN
            ILPON(17)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'E'     ) THEN
            ILPON(18)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'VISC-T') THEN
            ILPON(19)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'IPVC'  ) THEN
            ILPON(20)=ISW
          ELSEIF (TEXT(IS(4):IE(4)).EQ.'PPPVC' ) THEN
            ILPON(21)=ISW
          ELSE
            CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
          ENDIF
C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
        ENDIF

CD    -- FILE GRPを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'GRP') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
C       * FILE GRP STEP
        IF     (TEXT(IS(3):IE(3)).EQ.'STEP') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOI(IGRTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOI(IGRTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(IGRTRN(3),TEXT(IS(6):IE(6)))
          IGRTYP=1
          IF (IGRTRN(1).GT.IGRTRN(2) .OR. IGRTRN(3).LE.0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE GRP TIME
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'TIME') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOR(RGRTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(RGRTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(RGRTRN(3),TEXT(IS(6):IE(6)))
          IGRTYP=2
          IF (RGRTRN(1).GT.RGRTRN(2) .OR. RGRTRN(3).LE.0.0D0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE GRP AREA
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'AREA') THEN
          IF (NWD.LT.9) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          DO 100 I=1,6
            CALL VF_ZSTOI(IGRARA(I),TEXT(IS(I+3):IE(I+3)))
            IGRARA(I)=IGRARA(I)+1
 100      CONTINUE
          IF (IGRARA(1).LE.1) IGRARA(1)=2
          IF (IGRARA(2).LE.1) IGRARA(2)=2
          IF (IGRARA(3).LE.1) IGRARA(3)=2
          IF (IGRARA(4).LE.1) IGRARA(4)=NUMI0-1
          IF (IGRARA(5).LE.1) IGRARA(5)=NUMJ0-1
          IF (IGRARA(6).LE.1) IGRARA(6)=NUMK -1
          IF ((IGRARA(1).GT.IGRARA(4)) .OR.
     &        (IGRARA(2).GT.IGRARA(5)) .OR.
     &        (IGRARA(3).GT.IGRARA(6)) .OR.
     &        (IGRARA(4).GE.NUMI0    ) .OR.
     &        (IGRARA(5).GE.NUMJ0    ) .OR.
     &        (IGRARA(6).GE.NUMK     )     )
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE GRP ON  VORT
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'ON' ) THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          IF (TEXT(IS(4):IE(4)).EQ.'VORT') THEN
            IGRVOR=1
          ELSE
            CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
          ENDIF
C       * FILE GRP OFF VORT
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'OFF') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          IF (TEXT(IS(4):IE(4)).EQ.'VORT') THEN
            IGRVOR=0
          ELSE
            CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
          ENDIF
C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
        ENDIF

CD    -- FILE RSLを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'RSL') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
C       * FILE RSL STEP
        IF     (TEXT(IS(3):IE(3)).EQ.'STEP') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOI(IRSTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOI(IRSTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(IRSTRN(3),TEXT(IS(6):IE(6)))
          IRSTYP=1
          IF (IRSTRN(1).GT.IRSTRN(2) .OR. IRSTRN(3).LE.0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE RSL TIME
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'TIME') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOR(RRSTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(RRSTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(RRSTRN(3),TEXT(IS(6):IE(6)))
          IRSTYP=2
          IF (RRSTRN(1).GT.RRSTRN(2) .OR. RRSTRN(3).LE.0.0D0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE RSL ELAPSE
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'ELAPSE') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOR(ETIME,TEXT(IS(4):IE(4)))

C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
        ENDIF

CD    -- FILE RESを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'RES') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
        CALL VF_ZSTOI(IRETYP,TEXT(IS(3):IE(3)))
        IF (IRETYP.LT.0) CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')

CD    -- FILE TRNを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'TRN') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
C       * FILE TRN STEP
        IF     (TEXT(IS(3):IE(3)).EQ.'STEP') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOI(ITRTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOI(ITRTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(ITRTRN(3),TEXT(IS(6):IE(6)))
          ITRTYP=1
          IF (ITRTRN(1).GT.ITRTRN(2) .OR. ITRTRN(3).LE.0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE TRN TIME
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'TIME') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOR(RTRTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(RTRTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(RTRTRN(3),TEXT(IS(6):IE(6)))
          ITRTYP=2
          IF (RTRTRN(1).GT.RTRTRN(2) .OR. RTRTRN(3).LE.0.0D0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE TRN W-LEVEL
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'W-LEVEL') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          IF (ITRNUM.GE.MAXTR)
     &                  CALL VF_A2ERR('VF_IIFILE','AREA IS FULL.')
          ITRNUM=ITRNUM+1
          ITRPRM(1,ITRNUM)=0
          ITRPRM(2,ITRNUM)=0
          IF (TEXT(IS(4):IE(4)).EQ.'ANS') THEN
            IF     (TEXT(IS(5):IE(5)).EQ.'X-') THEN
              ITRPRM(3,ITRNUM)=-1
            ELSEIF (TEXT(IS(5):IE(5)).EQ.'X+') THEN
              ITRPRM(3,ITRNUM)=-2
            ELSEIF (TEXT(IS(5):IE(5)).EQ.'Y-') THEN
              ITRPRM(3,ITRNUM)=-3
            ELSEIF (TEXT(IS(5):IE(5)).EQ.'Y+') THEN
              ITRPRM(3,ITRNUM)=-4
            ELSEIF (TEXT(IS(5):IE(5)).EQ.'SRC') THEN
              ITRPRM(3,ITRNUM)=-5
            ELSE
              CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
            ENDIF
            ITRPRM(4,ITRNUM)=0
          ELSE
            CALL VF_ZSTOI(I1,TEXT(IS(4):IE(4)))
            CALL VF_ZSTOI(J1,TEXT(IS(5):IE(5)))
            IF (I1.LT.1 .OR. I1.GT.NUMI0-2)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
            IF (J1.LT.1 .OR. J1.GT.NUMJ0-2)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
            ITRPRM(3,ITRNUM)=I1+1
            ITRPRM(4,ITRNUM)=J1+1
          ENDIF
          ITRPRM(5,ITRNUM)=0
          ITRPRM(6,ITRNUM)=0
          ITRPRM(7,ITRNUM)=0
          ITRPRM(8,ITRNUM)=0
C       * FILE TRN {CALC} {PHY} .......
        ELSE
          IF (ITRNUM.GE.MAXTR)
     &                  CALL VF_A2ERR('VF_IIFILE','AREA IS FULL.')
          ITRNUM=ITRNUM+1
C         * 算出方法
          IF     (TEXT(IS(3):IE(3)).EQ.'POINT') THEN
            IPM= 7
            IF (NWD.LT.IPM) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
            ITRPRM(1,ITRNUM)=1
          ELSEIF (TEXT(IS(3):IE(3)).EQ.'MIN'  ) THEN
            IPM=10
            IF (NWD.LT.IPM) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
            ITRPRM(1,ITRNUM)=2
          ELSEIF (TEXT(IS(3):IE(3)).EQ.'MAX'  ) THEN
            IPM=10
            IF (NWD.LT.IPM) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
            ITRPRM(1,ITRNUM)=3
          ELSEIF (TEXT(IS(3):IE(3)).EQ.'AV'   ) THEN
            IPM=10
            IF (NWD.LT.IPM) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
            ITRPRM(1,ITRNUM)=4
          ELSEIF (TEXT(IS(3):IE(3)).EQ.'INT'  ) THEN
            IPM=10
            IF (NWD.LT.IPM) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
            ITRPRM(1,ITRNUM)=5
          ELSEIF (TEXT(IS(3):IE(3)).EQ.'FORCE') THEN
            IPM=10
            IF (NWD.LT.IPM) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
            ITRPRM(1,ITRNUM)=11
          ELSE
            CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
          ENDIF
C         * 物理量
          IP=4
          IF (ITRPRM(1,ITRNUM).EQ.11) THEN
            IF     (TEXT(IS(4):IE(4)).EQ.'X-') THEN
              ITRPRM(2,ITRNUM)=11
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'X+') THEN
              ITRPRM(2,ITRNUM)=12
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'Y-') THEN
              ITRPRM(2,ITRNUM)=13
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'Y+') THEN
              ITRPRM(2,ITRNUM)=14
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'Z-') THEN
              ITRPRM(2,ITRNUM)=15
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'Z+') THEN
              ITRPRM(2,ITRNUM)=16
            ELSE
              CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
            ENDIF
          ELSE
            IF     (TEXT(IS(4):IE(4)).EQ.'U') THEN
              ITRPRM(2,ITRNUM)=1
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'V') THEN
              ITRPRM(2,ITRNUM)=2
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'W') THEN
              ITRPRM(2,ITRNUM)=3
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'P') THEN
              ITRPRM(2,ITRNUM)=4
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'F') THEN
              ITRPRM(2,ITRNUM)=5
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'K') THEN
              ITRPRM(2,ITRNUM)=6
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'E') THEN
              ITRPRM(2,ITRNUM)=7
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'T') THEN
              ITRPRM(2,ITRNUM)=8
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'C') THEN
              IF (NWD.LT.IPM+1)
     &                      CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
              CALL VF_ZSTOI(LC,TEXT(IS(5):IE(5)))
              IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                     CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
              ITRPRM(2,ITRNUM)=-LC
              IP=5
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'VORT-X') THEN
              ITRPRM(2,ITRNUM)=17
              IF (ITRPRM(1,ITRNUM).NE.1) 
     &                      CALL VF_A2ERR('VF_IIFILE','{PHY} ERROR.')
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'VORT-Y') THEN
              ITRPRM(2,ITRNUM)=18
              IF (ITRPRM(1,ITRNUM).NE.1) 
     &                      CALL VF_A2ERR('VF_IIFILE','{PHY} ERROR.')
            ELSEIF (TEXT(IS(4):IE(4)).EQ.'VORT-Z') THEN
              ITRPRM(2,ITRNUM)=19
              IF (ITRPRM(1,ITRNUM).NE.1) 
     &                      CALL VF_A2ERR('VF_IIFILE','{PHY} ERROR.')
            ELSE
              CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
            ENDIF
          ENDIF
C         * 演算範囲
          I2=-1
          J2=-1
          K2=-1
          CALL VF_ZSTOI(I1,TEXT(IS(IP+1):IE(IP+1)))
          CALL VF_ZSTOI(J1,TEXT(IS(IP+2):IE(IP+2)))
          CALL VF_ZSTOI(K1,TEXT(IS(IP+3):IE(IP+3)))
          IF     (ITRPRM(1,ITRNUM).EQ.1 .AND.
     &            ITRPRM(2,ITRNUM).EQ.1      ) THEN
            IF (I1.LT.1 .OR. I1.GT.NUMI0-1 .OR.
     &          J1.LT.1 .OR. J1.GT.NUMJ0-2 .OR.
     &          K1.LT.1 .OR. K1.GT.NUMK -2     )
     &          CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ELSEIF (ITRPRM(1,ITRNUM).EQ.1 .AND.
     &            ITRPRM(2,ITRNUM).EQ.2      ) THEN
            IF (I1.LT.1 .OR. I1.GT.NUMI0-2 .OR.
     &          J1.LT.1 .OR. J1.GT.NUMJ0-1 .OR.
     &          K1.LT.1 .OR. K1.GT.NUMK -2     )
     &          CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ELSEIF (ITRPRM(1,ITRNUM).EQ.1 .AND.
     &            ITRPRM(2,ITRNUM).EQ.3      ) THEN
            IF (I1.LT.1 .OR. I1.GT.NUMI0-2 .OR.
     &          J1.LT.1 .OR. J1.GT.NUMJ0-2 .OR.
     &          K1.LT.1 .OR. K1.GT.NUMK -1     )
     &          CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ELSE
            IF (I1.LT.1 .OR. I1.GT.NUMI0-2 .OR.
     &          J1.LT.1 .OR. J1.GT.NUMJ0-2 .OR.
     &          K1.LT.1 .OR. K1.GT.NUMK -2     )
     &          CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ENDIF
          IF (ITRPRM(1,ITRNUM).NE.1) THEN
            CALL VF_ZSTOI(I2,TEXT(IS(IP+4):IE(IP+4)))
            CALL VF_ZSTOI(J2,TEXT(IS(IP+5):IE(IP+5)))
            CALL VF_ZSTOI(K2,TEXT(IS(IP+6):IE(IP+6)))
            IF (I2.LT.I1 .OR. I2.GT.NUMI0-2 .OR.
     &          J2.LT.J1 .OR. J2.GT.NUMJ0-2 .OR.
     &          K2.LT.K1 .OR. K2.GT.NUMK -2     )
     &          CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
          ENDIF
          ITRPRM(3,ITRNUM)=I1+1
          ITRPRM(4,ITRNUM)=J1+1
          ITRPRM(5,ITRNUM)=K1+1
          ITRPRM(6,ITRNUM)=I2+1
          ITRPRM(7,ITRNUM)=J2+1
          ITRPRM(8,ITRNUM)=K2+1
        ENDIF

CD    -- FILE MAMを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'MAM') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
C       * FILE MAM STEP
        IF     (TEXT(IS(3):IE(3)).EQ.'STEP') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOI(IMMTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOI(IMMTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOI(IMMTRN(3),TEXT(IS(6):IE(6)))
          IMMTYP=1
          IF (IMMTRN(1).GT.IMMTRN(2) .OR. IMMTRN(3).LE.0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * FILE MAM TIME
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'TIME') THEN
          IF (NWD.LT.6) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
          CALL VF_ZSTOR(RMMTRN(1),TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(RMMTRN(2),TEXT(IS(5):IE(5)))
          CALL VF_ZSTOR(RMMTRN(3),TEXT(IS(6):IE(6)))
          IMMTYP=2
          IF (RMMTRN(1).GT.RMMTRN(2) .OR. RMMTRN(3).LE.0.0D0)
     &                 CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')
C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
        ENDIF

CD    -- FILE POROを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'PORO') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IIFILE','SYNTAX ERROR.')
        CALL VF_ZSTOI(IPRNT,TEXT(IS(3):IE(3)))
        IF (IPRNT.LT.0) CALL VF_A2ERR('VF_IIFILE','INVALID VALUE.')

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IIFILE','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
