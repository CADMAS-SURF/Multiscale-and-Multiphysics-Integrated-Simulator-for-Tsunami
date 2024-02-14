      SUBROUTINE VF_OLBC(BCU,BCV,BCW,BCP,BCF,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                   INDB,INDBK,INDBE,INDBT,INDBC)

CD=== 概要 ===========================================================

CDT   VF_OLBC:境界条件をリストファイルに出力

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BCU(NUMB,3)        : IN : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)        : IN : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)        : IN : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)        : IN : R*8 : 圧力の境界値
CD    BCF(NUMB)          : IN : R*8 : VOF関数Fの境界値
CD    BCK(NUMB,3)        : IN : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)        : IN : R*8 : 乱流エネルギ散逸の境界値
CD    BCT(NUMB)          : IN : R*8 : 温度の境界値
CD    BCTI(2,NUMB)       : IN : R*8 : 温度の境界条件
CD    BCC(NUMB,LEQC)     : IN : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC)  : IN : R*8 : 濃度の境界条件
CD    INDB(MAXB1,NUMB)   : IN : I*4 : 境界面のインデックス
CD    INDBK(MAXBK1,NUMB) : IN : I*4 : 乱流エネルギの境界条件
CD    INDBE(MAXBE1,NUMB) : IN : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)        : IN : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC)   : IN : I*4 : 濃度の境界条件
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCP(NUMB,3)
      DIMENSION BCF(NUMB),BCK(NUMB,3),BCE(NUMB,3)
      DIMENSION BCT(NUMB),BCTI(2,NUMB)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION INDB(MAXB1,NUMB),INDBK(MAXBK1,NUMB),INDBE(MAXBE1,NUMB)
      DIMENSION INDBT(NUMB),INDBC(NUMB,LEQC)

CD    -- 局所変数 --
      CHARACTER*6 TEXT1,TEXT2,TEXT3,TEXT4

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- リストファイルに出力 --
      WRITE(ILPFIL,9510)
     &            'INDXYZ','I','J','K','DIREC',
     &            '      ','VEL-X','VEL-Y','VEL-Z','PRESS',
     &            'B.C-VP','VEL-X','VEL-Y','VEL-Z',
     &            'B.C-VG','VEL-X','VEL-Y','VEL-Z',
     &            'B.C-F' ,'F'
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        IF (IJK.LT.0) GOTO 100
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
C       * 液相 *
        IBV=INDB(3,L)
        IF (IBV.EQ.0) THEN
          TEXT1='UNDEF'
        ELSEIF (IBV.EQ.1) THEN
          TEXT1='SLIP'
        ELSEIF (IBV.EQ.2) THEN
          TEXT1='NON-S'
        ELSEIF (IBV.EQ.3) THEN
          TEXT1='FIX-V'
        ELSEIF (IBV.EQ.4) THEN
          TEXT1='FREE'
        ELSEIF (IBV.EQ.5) THEN
          TEXT1='WAVE'
        ELSEIF (IBV.EQ.6) THEN
          TEXT1='LOG'
        ELSEIF (IBV.EQ.7) THEN
          TEXT1='OPEN1'
        ELSEIF (IBV.EQ.8) THEN
          TEXT1='LOG-KS'
        ELSE
          CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
        ENDIF
C       * 気相 *
        IBV=INDB(5,L)
        IF (IBV.EQ.0) THEN
          TEXT2='UNDEF'
        ELSEIF (IBV.EQ.1) THEN
          TEXT2='SLIP'
        ELSEIF (IBV.EQ.2) THEN
          TEXT2='NON-S'
        ELSEIF (IBV.EQ.3) THEN
          TEXT2='FIX-V'
        ELSEIF (IBV.EQ.4) THEN
          TEXT2='FREE'
        ELSEIF (IBV.EQ.5) THEN
          TEXT2='WAVE'
        ELSEIF (IBV.EQ.6) THEN
          TEXT2='LOG'
        ELSEIF (IBV.EQ.7) THEN
          TEXT2='OPEN1'
        ELSEIF (IBV.EQ.8) THEN
          TEXT2='LOG-KS'
        ELSE
          CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
        ENDIF
C       * F *
        IBF=INDB(4,L)
        IF (IBF.EQ.0) THEN
          TEXT3='UNDEF'
        ELSEIF (IBF.EQ.1) THEN
          TEXT3='FIX'
        ELSEIF (IBF.EQ.2) THEN
          TEXT3='FREE'
        ELSEIF (IBF.EQ.5) THEN
          TEXT3='WAVE'
        ELSEIF (IBF.EQ.7) THEN
          TEXT3='OPEN1'
        ELSE
          CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
        ENDIF
C       * 出力 *
        WRITE(ILPFIL,9520)
     &              SIGN(L,IJK),I-1+IP,J-1+JP,K-1,INDB(2,L),
     &              'MIX',BCU(L,1),BCV(L,1),BCW(L,1),BCP(L,1),
     &              TEXT1,BCU(L,2),BCV(L,2),BCW(L,2),
     &              TEXT2,BCU(L,3),BCV(L,3),BCW(L,3),
     &              TEXT3,BCF(L)
 100  CONTINUE

      IF (LEQK.NE.0) THEN
        WRITE(ILPFIL,9530) 'INDXYZ','I','J','K','DIREC',
     &                     'B.C-K','K','B.C-E','E',
     &                     'LIQ-K','K','LIQ-E','E',
     &                     'GAS-K','K','GAS-E','E'
        DO 200 L=1,NUMB
          IJK=INDB(1,L)
          IF (IJK.LE.0) GOTO 200
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
C         * 液相 *
          IBV=INDBK(1,L)
          IF     (IBV.EQ. 0) THEN
            TEXT1='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXT1='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXT1='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXT1='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXT1='FREE+A'
          ELSEIF (IBV.EQ. 6) THEN
            TEXT1='LOG'
          ELSEIF (IBV.EQ. 8) THEN
            TEXT1='LOG-KS'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          IBF=INDBE(1,L)
          IF     (IBF.EQ. 0) THEN
            TEXT2='UNDEF'
          ELSEIF (IBF.EQ.-1) THEN
            TEXT2='FIX-A'
          ELSEIF (IBF.EQ. 1) THEN
            TEXT2='FIX+A'
          ELSEIF (IBF.EQ.-2) THEN
            TEXT2='FREE-A'
          ELSEIF (IBF.EQ. 2) THEN
            TEXT2='FREE+A'
          ELSEIF (IBF.EQ. 6) THEN
            TEXT2='LOG'
          ELSEIF (IBF.EQ. 8) THEN
            TEXT2='LOG-KS'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
C         * 気相 *
          IBV=INDBK(2,L)
          IF     (IBV.EQ. 0) THEN
            TEXT3='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXT3='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXT3='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXT3='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXT3='FREE+A'
          ELSEIF (IBV.EQ. 6) THEN
            TEXT3='LOG'
          ELSEIF (IBV.EQ. 8) THEN
            TEXT3='LOG-KS'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          IBF=INDBE(2,L)
          IF     (IBF.EQ. 0) THEN
            TEXT4='UNDEF'
          ELSEIF (IBF.EQ.-1) THEN
            TEXT4='FIX-A'
          ELSEIF (IBF.EQ. 1) THEN
            TEXT4='FIX+A'
          ELSEIF (IBF.EQ.-2) THEN
            TEXT4='FREE-A'
          ELSEIF (IBF.EQ. 2) THEN
            TEXT4='FREE+A'
          ELSEIF (IBF.EQ. 6) THEN
            TEXT4='LOG'
          ELSEIF (IBF.EQ. 8) THEN
            TEXT4='LOG-KS'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
C         * 出力 *
          WRITE(ILPFIL,9540) L,I-1+IP,J-1+JP,K-1,INDB(2,L),
     &                         'MIX',BCK(L,1),'   ',BCE(L,1),
     &                         TEXT1,BCK(L,2),TEXT2,BCE(L,2),
     &                         TEXT3,BCK(L,3),TEXT4,BCE(L,3)
 200      CONTINUE
 210    CONTINUE
      ENDIF

      IF (LEQT.NE.0) THEN
        WRITE(ILPFIL,9550) 'INDXYZ','I','J','K','DIREC',
     &                     'B.C-T','TEMPERATURE','BTQ/BTH','BT0'
        DO 300 L=1,NUMB
          IJK=INDB(1,L)
          IF (IJK.LE.0) GOTO 300
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IBV=INDBT(L)
          IF     (IBV.EQ. 0) THEN
            TEXT1='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXT1='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXT1='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXT1='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXT1='FREE+A'
          ELSEIF (IBV.EQ.-3) THEN
            TEXT1='FLUX-A'
          ELSEIF (IBV.EQ. 3) THEN
            TEXT1='FLUX+A'
          ELSEIF (IBV.EQ.-4) THEN
            TEXT1='TRAN-A'
          ELSEIF (IBV.EQ. 4) THEN
            TEXT1='TRAN+A'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          WRITE(ILPFIL,9570) L,I-1,J-1+IP,K-1+JP,INDB(2,L),
     &                       TEXT1,BCT(L),BCTI(1,L),BCTI(2,L)
 300    CONTINUE
      ENDIF

      DO 410 LC=1,LEQC
        WRITE(ILPFIL,9560) 'INDXYZ','I','J','K','DIREC',
     &                     'B.C-C','CONC',LC,'BCQ/BCH','BC0'
        DO 400 L=1,NUMB
          IJK=INDB(1,L)
          IF (IJK.LE.0) GOTO 400
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IBV=INDBC(L,LC)
          IF     (IBV.EQ. 0) THEN
            TEXT1='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXT1='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXT1='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXT1='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXT1='FREE+A'
          ELSEIF (IBV.EQ.-3) THEN
            TEXT1='FLUX-A'
          ELSEIF (IBV.EQ. 3) THEN
            TEXT1='FLUX+A'
          ELSEIF (IBV.EQ.-4) THEN
            TEXT1='TRAN-A'
          ELSEIF (IBV.EQ. 4) THEN
            TEXT1='TRAN+A'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          WRITE(ILPFIL,9570) L,I-1,J-1+IP,K-1+JP,INDB(2,L),
     &                       TEXT1,BCC(L,LC),BCCI(1,L,LC),BCCI(2,L,LC)
 400    CONTINUE
 410  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',A8,4A6,
     &        ' ',A6,' ',A8   ,' ',A8 ,' ',A8  ,' ',A8  ,
     &        ' ',A6,' ',A8   ,' ',A8 ,' ',A8  ,
     &        ' ',A6,' ',A8   ,' ',A8 ,' ',A8  ,
     &        ' ',A6,' ',A6)
 9520 FORMAT( ' ',I8,4I6,1P,
     &        ' ',A6,' ',E8.1,' ',E8.1,' ',E8.1,' ',E8.1,
     &        ' ',A6,' ',E8.1,' ',E8.1,' ',E8.1,
     &        ' ',A6,' ',E8.1,' ',E8.1,' ',E8.1,
     &        ' ',A6,' ',E8.1)
 9530 FORMAT(/' ',A8,4A6,
     &        ' ',A6,' ',A8  ,' ',A6,' ',A8  ,
     &        ' ',A6,' ',A8  ,' ',A6,' ',A8  ,
     &        ' ',A6,' ',A8  ,' ',A6,' ',A8  )
 9540 FORMAT( ' ',I8,4I6,1P,
     &        ' ',A6,' ',E8.1,' ',A6,' ',E8.1,
     &        ' ',A6,' ',E8.1,' ',A6,' ',E8.1,
     &        ' ',A6,' ',E8.1,' ',A6,' ',E8.1)
 9550 FORMAT(/' ',A8,4A6,' ',A6,' ',A12         ,' ',A12    ,' ',A12)
 9560 FORMAT(/' ',A8,4A6,' ',A6,' ',A4,'('I6,')',' ',A12    ,' ',A12)
 9570 FORMAT( ' ',I8,4I6,' ',A6,' ',1PE12.5 ,' ',1PE12.5,' ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
