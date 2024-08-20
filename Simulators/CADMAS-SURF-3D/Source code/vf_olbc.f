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
CD    BCU(NUMB)        : IN : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : IN : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : IN : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : IN : R*8 : 圧力の境界値
CD    BCF(NUMB)        : IN : R*8 : VOF関数Fの境界値
CD    BCK(NUMB)        : IN : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : IN : R*8 : 乱流エネルギ散逸の境界値
CD    BCT(NUMB)        : IN : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : IN : R*8 : 温度の境界条件
CD    BCC(NUMB,LEQC)    : IN : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : IN : R*8 : 濃度の境界条件
CD    INDB(MAXB1,NUMB) : IN : I*4 : 境界面のインデックス
CD    INDBK(NUMB)      : IN : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : IN : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : IN : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : IN : I*4 : 濃度の境界条件
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION BCK(NUMB),BCE(NUMB),BCT(NUMB),BCTI(2,NUMB)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION INDB(MAXB1,NUMB),INDBK(NUMB),INDBE(NUMB)
      DIMENSION INDBT(NUMB),INDBC(NUMB,LEQC)

CD    -- 局所変数 --
      CHARACTER*6 TEXTV,TEXTF

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- リストファイルに出力 --
      WRITE(ILPFIL,9510) 'INDXYZ','I','J','K','DIREC',
     &                   'B.C-VP','VELOCITY-X','VELOCITY-Y',
     &                   'VELOCITY-Z','PRESSURE',
     &                   'B.C-F' ,'F'
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        IBV=INDB(3,L)
        IF (IBV.EQ.0) THEN
          TEXTV='UNDEF'
        ELSEIF (IBV.EQ.1) THEN
          TEXTV='SLIP'
        ELSEIF (IBV.EQ.2) THEN
          TEXTV='NON-S'
        ELSEIF (IBV.EQ.3) THEN
          TEXTV='FIX-V'
        ELSEIF (IBV.EQ.4) THEN
          TEXTV='FREE'
        ELSEIF (IBV.EQ.5) THEN
          TEXTV='WAVE'
        ELSEIF (IBV.EQ.6) THEN
          TEXTV='LOG'
        ELSEIF (IBV.EQ.7) THEN
          TEXTV='OPEN1'
        ELSEIF (IBV.EQ.8) THEN
          TEXTV='LOG-KS'
        ELSE
          CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
        ENDIF
        IBF=INDB(4,L)
        IF (IBF.EQ.0) THEN
          TEXTF='UNDEF'
        ELSEIF (IBF.EQ.1) THEN
          TEXTF='FIX'
        ELSEIF (IBF.EQ.2) THEN
          TEXTF='FREE'
        ELSEIF (IBF.EQ.5) THEN
          TEXTF='WAVE'
        ELSEIF (IBF.EQ.7) THEN
          TEXTF='OPEN1'
        ELSE
          CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
        ENDIF
        WRITE(ILPFIL,9520) L,I-1+IP,J-1+JP,K-1,INDB(2,L),
     &                     TEXTV,BCU(L),BCV(L),BCW(L),BCP(L),
     &                     TEXTF,BCF(L)
 100  CONTINUE

      IF (LEQK.NE.0) THEN
        WRITE(ILPFIL,9530) 'INDXYZ','I','J','K','DIREC',
     &                     'B.C-K','K','B.C-E','E'
        DO 200 L=1,NUMB
          IJK=INDB(1,L)
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IBV=INDBK(L)
          IF     (IBV.EQ. 0) THEN
            TEXTV='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXTV='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXTV='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXTV='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXTV='FREE+A'
          ELSEIF (IBV.EQ. 6) THEN
            TEXTV='LOG'
          ELSEIF (IBV.EQ. 8) THEN
            TEXTV='LOG-KS'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          IBF=INDBE(L)
          IF     (IBF.EQ. 0) THEN
            TEXTF='UNDEF'
          ELSEIF (IBF.EQ.-1) THEN
            TEXTF='FIX-A'
          ELSEIF (IBF.EQ. 1) THEN
            TEXTF='FIX+A'
          ELSEIF (IBF.EQ.-2) THEN
            TEXTF='FREE-A'
          ELSEIF (IBF.EQ. 2) THEN
            TEXTF='FREE+A'
          ELSEIF (IBF.EQ. 6) THEN
            TEXTF='LOG'
          ELSEIF (IBF.EQ. 8) THEN
            TEXTF='LOG-KS'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          WRITE(ILPFIL,9540) L,I-1+IP,J-1+JP,K-1,INDB(2,L),
     &                       TEXTV,BCK(L),TEXTF,BCE(L)
 200    CONTINUE
      ENDIF

      IF (LEQT.NE.0) THEN
        WRITE(ILPFIL,9550) 'INDXYZ','I','J','K','DIREC',
     &                     'B.C-T','TEMPERATURE','BTQ/BTH','BT0'
        DO 300 L=1,NUMB
          IJK=INDB(1,L)
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IBV=INDBT(L)
          IF     (IBV.EQ. 0) THEN
            TEXTV='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXTV='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXTV='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXTV='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXTV='FREE+A'
          ELSEIF (IBV.EQ.-3) THEN
            TEXTV='FLUX-A'
          ELSEIF (IBV.EQ. 3) THEN
            TEXTV='FLUX+A'
          ELSEIF (IBV.EQ.-4) THEN
            TEXTV='TRAN-A'
          ELSEIF (IBV.EQ. 4) THEN
            TEXTV='TRAN+A'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          WRITE(ILPFIL,9570) L,I-1,J-1+IP,K-1+JP,INDB(2,L),
     &                       TEXTV,BCT(L),BCTI(1,L),BCTI(2,L)
 300    CONTINUE
      ENDIF

      DO 410 LC=1,LEQC
        WRITE(ILPFIL,9560) 'INDXYZ','I','J','K','DIREC',
     &                     'B.C-C','CONC',LC,'BCQ/BCH','BC0'
        DO 400 L=1,NUMB
          IJK=INDB(1,L)
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IBV=INDBC(L,LC)
          IF     (IBV.EQ. 0) THEN
            TEXTV='UNDEF'
          ELSEIF (IBV.EQ.-1) THEN
            TEXTV='FIX-A'
          ELSEIF (IBV.EQ. 1) THEN
            TEXTV='FIX+A'
          ELSEIF (IBV.EQ.-2) THEN
            TEXTV='FREE-A'
          ELSEIF (IBV.EQ. 2) THEN
            TEXTV='FREE+A'
          ELSEIF (IBV.EQ.-3) THEN
            TEXTV='FLUX-A'
          ELSEIF (IBV.EQ. 3) THEN
            TEXTV='FLUX+A'
          ELSEIF (IBV.EQ.-4) THEN
            TEXTV='TRAN-A'
          ELSEIF (IBV.EQ. 4) THEN
            TEXTV='TRAN+A'
          ELSE
            CALL VF_A2ERR('VF_OLBC','P.G ERROR.')
          ENDIF
          WRITE(ILPFIL,9570) L,I-1,J-1+IP,K-1+JP,INDB(2,L),
     &                       TEXTV,BCC(L,LC),BCCI(1,L,LC),BCCI(2,L,LC)
 400    CONTINUE
 410  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',A8,4A6,' ',A6,' ',A12    ,' ',A12    ,
     &                          ' ',A12    ,' ',A12    ,
     &                   ' ',A6,' ',A12    )
 9520 FORMAT( ' ',I8,4I6,' ',A6,' ',1PE12.5,' ',1PE12.5,
     &                          ' ',1PE12.5,' ',1PE12.5,
     &                   ' ',A6,' ',1PE12.5)
 9530 FORMAT(/' ',A8,4A6,' ',A6,' ',A12    ,' ',A6,' ',A12    )
 9540 FORMAT( ' ',I8,4I6,' ',A6,' ',1PE12.5,' ',A6,' ',1PE12.5)
 9550 FORMAT(/' ',A8,4A6,' ',A6,' ',A12         ,' ',A12    ,' ',A12)
 9560 FORMAT(/' ',A8,4A6,' ',A6,' ',A4,'('I6,')',' ',A12    ,' ',A12)
 9570 FORMAT( ' ',I8,4I6,' ',A6,' ',1PE12.5 ,' ',1PE12.5,' ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
