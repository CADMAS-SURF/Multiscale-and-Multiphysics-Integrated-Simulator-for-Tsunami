      SUBROUTINE VF_IP1INP(T0,GGVOLD,GGVNOW)

CD=== 概要 ===========================================================

CDT   VF_IP1INP:時間依存型空隙率ファイルを読み込む

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    T0            : IN  : R*8 : 対象とする時刻
CD    GGVOLD(IPRNP) : I/O : R*8 : 前の時刻ブロックの空隙率
CD    GGVNOW(IPRNP) : I/O : R*8 : 現在の時刻ブロックの空隙率
      DIMENSION GGVOLD(IPRNP),GGVNOW(IPRNP)

C==== 実行 ===========================================================

CD    -- 初期ブロックを読み込む --
      IF (IPRNT.GE.1 .AND. IPRIT.LT.0) THEN
        IPRFIL=0
        IF (MYRANK.EQ.0) THEN
          OPEN(MFILPR,ERR=9010,
     &         FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.poro',
     &         STATUS='OLD',FORM='UNFORMATTED' )
          IPRFIL=MFILPR
          WRITE(ILPFIL,9510)
          READ(IPRFIL,END=9020,ERR=9030) IPRNB,ISW1,ISW2,ISW3,ISW4
        ENDIF
        IF (NPROCS.NE.1) THEN
          CALL VF_P1BCSI(IPRNB,1,0)
        ENDIF
        IF (IPRNB.GT.MAXPRB) CALL VF_A2ERR('VF_IP1INP','AREA IS FULL.')
        IPRNP=0
        DO 110 IB=1,IPRNB
          IF (MYRANK.EQ.0) THEN
            READ(IPRFIL,END=9020,ERR=9030) (IPRARA(I,IB),I=1,6)
          ENDIF
          IF (NPROCS.NE.1) THEN
            CALL VF_P1BCSI(IPRARA(1,IB),6,0)
          ENDIF
          DO 100 I=1,6
            IPRARA(I,IB)=IPRARA(I,IB)+1
 100      CONTINUE
          IF ((IPRARA(1,IB).GT.IPRARA(4,IB)) .OR.
     &        (IPRARA(2,IB).GT.IPRARA(5,IB)) .OR.
     &        (IPRARA(3,IB).GT.IPRARA(6,IB)) .OR.
     &        (IPRARA(1,IB).LE.1           ) .OR.
     &        (IPRARA(2,IB).LE.1           ) .OR.
     &        (IPRARA(3,IB).LE.1           ) .OR.
     &        (IPRARA(4,IB).GE.NUMI0       ) .OR.
     &        (IPRARA(5,IB).GE.NUMJ0       ) .OR.
     &        (IPRARA(6,IB).GE.NUMK        )     )
     &                 CALL VF_A2ERR('VF_IP1INP','INVALID VALUE.')
          IPRNP=IPRNP+ (IPRARA(4,IB)-IPRARA(1,IB)+1)
     &                *(IPRARA(5,IB)-IPRARA(2,IB)+1)
     &                *(IPRARA(6,IB)-IPRARA(3,IB)+1)
 110    CONTINUE
        IPRIT=0

CD    -- 第I時刻ブロックを読み込む --
      ELSEIF (IPRNT.GE.1) THEN
        IF (MYRANK.EQ.0) THEN
 200      CONTINUE
            IF ((IPRIT.NE.0 .AND. PRTNOW.GE.T0) .OR.
     &          IPRNT.LE.IPRIT                      ) GOTO 240
            IPRIT=IPRIT+1
            PRTOLD=PRTNOW
            DO 210 L=1,IPRNP
              GGVOLD(L)=GGVNOW(L)
 210        CONTINUE
            READ(IPRFIL,END=9020,ERR=9030) PRTNOW,JSW1,JSW2,JSW3,JSW4
            WRITE(ILPFIL,9520) IPRIT,PRTNOW
            NP=0
            DO 220 IB=1,IPRNB
              NI=IPRARA(4,IB)-IPRARA(1,IB)+1
              NJ=IPRARA(5,IB)-IPRARA(2,IB)+1
              NK=IPRARA(6,IB)-IPRARA(3,IB)+1
              READ(IPRFIL,END=9020,ERR=9030)
     &               (((GGVNOW(NP+I+NI*(J-1)+NI*NJ*(K-1)),
     &                  I=1,NI),J=1,NJ),K=1,NK)
              NP=NP+NI*NJ*NK
 220        CONTINUE
            DO 230 L=1,IPRNP
              GGVNOW(L)=MAX(GGVNOW(L),PLOWER)
 230        CONTINUE
            GOTO 200
 240      CONTINUE
          IF (IPRIT.EQ.1) THEN
            PRTOLD=PRTNOW
            DO 250 L=1,IPRNP
              GGVOLD(L)=GGVNOW(L)
 250        CONTINUE
          ENDIF
        ENDIF
        IF (NPROCS.NE.1) THEN
          CALL VF_P1BCSI(IPRIT ,    1,0)
          CALL VF_P1BCSD(PRTOLD,    1,0)
          CALL VF_P1BCSD(PRTNOW,    1,0)
          CALL VF_P1BCSD(GGVOLD,IPRNP,0)
          CALL VF_P1BCSD(GGVNOW,IPRNP,0)
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_IP1INP','CAN NOT OPEN (*****.poro).')
      GOTO 9999

 9020 CONTINUE
      CALL VF_A2ERR('VF_IP1INP','END OF FILE (*****.poro).')
      GOTO 9999

 9030 CONTINUE
      CALL VF_A2ERR('VF_IP1INP','I/O ERROR (*****.poro).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-PORO : IN : INITIAL')
 9520 FORMAT( ' ','>> FILE-PORO : IN : IPRIT=',I6,' : PTIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
