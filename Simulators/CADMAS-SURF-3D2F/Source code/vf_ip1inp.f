      SUBROUTINE VF_IP1INP(T0,GGVOLD,GGVNOW,GGVLO,GGVLN)

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
CD    T0             : IN  : R*8 : 対象とする時刻
CD    GGVOLD(IPRNP)  : I/O : R*8 : 前の時刻ブロックの空隙率
CD    GGVNOW(IPRNP)  : I/O : R*8 : 現在の時刻ブロックの空隙率
CD    GGVLO(3,IPRNB) : I/O : R*8 : 前の時刻ブロックの障害物移動速度
CD    GGVLO(3,IPRNB) : I/O : R*8 : 現在の時刻ブロックの障害物移動速度
      DIMENSION GGVOLD(IPRNP),GGVNOW(IPRNP)
      DIMENSION GGVLO(3,IPRNB),GGVLN(3,IPRNB)

CD    -- 主要制御変数 --
CD    IPRNT : I*4 : 空隙率の時間方向のデータ数
CD                  =0:空隙率ファイルを読み込まない
CD                  =1:時間依存データでは無い
CD                  >1:時間依存データ
CD    IPRNB : I*4 : 時間依存型空隙率の空間ブロックの最大数
CD    IPRNP : I*4 : 時間依存型空隙率の設定セル数

C==== 実行 ===========================================================

CD    -- 空隙率ファイルの書式の選択 --
      JPRFIL=1

CD    -- 初期ブロックを読み込む --
      IF (IPRNT.GE.1 .AND. IPRIT.LT.0) THEN
        IPRFIL=0
        IF (MYRANK.EQ.0) THEN
          IF (JPRFIL.EQ.0) THEN
C----------------------------------------------------for MG/2FC coupling
C           OPEN(MFILPR,ERR=9010,FILE='data.poro',
            OPEN(MFILPR,ERR=9010,FILE=TRIM(MGNAME(MGRANK+1))//'.poro',
C----------------------------------------------------for MG/2FC coupling
     &                  STATUS='OLD',FORM='UNFORMATTED' )
            IPRFIL=MFILPR
            WRITE(ILPFIL,9510)
            READ(IPRFIL,END=9020,ERR=9030) IPRNB,ISW1,ISW2,ISW3,ISW4
CC            WRITE(2001,'(A,5I)') '(H1):',IPRNB,ISW1,ISW2,ISW3,ISW4
          ELSE
C----------------------------------------------------for MG/2FC coupling
C           OPEN(MFILPR,ERR=9010,FILE='data.poro.ascii',
            OPEN(MFILPR,ERR=9010,
     &                  FILE=TRIM(MGNAME(MGRANK+1))//'.poro.ascii',
C----------------------------------------------------for MG/2FC coupling
     &                  STATUS='OLD',FORM='FORMATTED' )
            IPRFIL=MFILPR
            WRITE(ILPFIL,9510)
            READ(IPRFIL,*,END=9020,ERR=9030) IPRNB,ISW1,ISW2,ISW3,ISW4
CC            WRITE(2001,'(A,5I)') '(H1):',IPRNB,ISW1,ISW2,ISW3,ISW4
          ENDIF
        ENDIF
        IF (NPROCS.NE.1) THEN
          CALL VF_P1BCSI(IPRNB,1,0)
        ENDIF
        IF (IPRNB.GT.MAXPRB) CALL VF_A2ERR('VF_IP1INP','AREA IS FULL.')
        IPRNP=0
        DO 110 IB=1,IPRNB
          IF (MYRANK.EQ.0) THEN
            IF (JPRFIL.EQ.0) THEN
              READ(IPRFIL,END=9020,ERR=9030) (IPRARA(I,IB),I=1,6)
CC              WRITE(2001,'(A,I3,A,6I)')
CC     &                    '(H2)-',IB,':',(IPRARA(I,IB),I=1,6)
            ELSE
              READ(IPRFIL,*,END=9020,ERR=9030) (IPRARA(I,IB),I=1,6)
CC              WRITE(2001,'(A,I3,A,6I)')
CC     &                    '(H2)-',IB,':',(IPRARA(I,IB),I=1,6)
            ENDIF
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
 210      CONTINUE
          DO 211 L=1,IPRNB
            GGVLO(1,L)=GGVLN(1,L)
            GGVLO(2,L)=GGVLN(2,L)
            GGVLO(3,L)=GGVLN(3,L)
 211      CONTINUE
          IF (JPRFIL.EQ.0) THEN
            READ(IPRFIL,END=9020,ERR=9030) PRTNOW,JSW1,JSW2,JSW3,JSW4
CC            WRITE(2001,'(A,1P,E10.2,4I)')
CC     &                  '(T ):',PRTNOW,JSW1,JSW2,JSW3,JSW4
          ELSE
            READ(IPRFIL,*,END=9020,ERR=9030) PRTNOW,JSW1,JSW2,JSW3,JSW4
CC            WRITE(2001,'(A,1P,E10.2,4I)')
CC     &                  '(T ):',PRTNOW,JSW1,JSW2,JSW3,JSW4
          ENDIF
          WRITE(ILPFIL,9520) IPRIT,PRTNOW
          NP=0
          DO 220 IB=1,IPRNB
            NI=IPRARA(4,IB)-IPRARA(1,IB)+1
            NJ=IPRARA(5,IB)-IPRARA(2,IB)+1
            NK=IPRARA(6,IB)-IPRARA(3,IB)+1
            IF (JPRFIL.EQ.0) THEN
              READ(IPRFIL,END=9020,ERR=9030)
     &               (((GGVNOW(NP+I+NI*(J-1)+NI*NJ*(K-1)),
     &                  I=1,NI),J=1,NJ),K=1,NK)
CC              WRITE(2001,'(A,I3,A)') '(D1)-',IB,':'
CC              WRITE(2001,'(1P,(10E10.2))')
CC     &                    (((GGVNOW(NP+I+NI*(J-1)+NI*NJ*(K-1)),
CC     &                    I=1,NI),J=1,NJ),K=1,NK)
              READ(IPRFIL,END=9020,ERR=9030) (GGVLN(I,IB),I=1,3)
CC              WRITE(2001,'(A,I3,A,1P,3E10.2)')
CC     &                    '(D2)-',IB,':',(GGVLN(I,IB),I=1,3)
            ELSE
              READ(IPRFIL,*,END=9020,ERR=9030)
     &               (((GGVNOW(NP+I+NI*(J-1)+NI*NJ*(K-1)),
     &                  I=1,NI),J=1,NJ),K=1,NK)
CC              WRITE(2001,'(A,I3,A)') '(D1)-',IB,':'
CC              WRITE(2001,'(1P,(10E10.2))')
CC     &                    (((GGVNOW(NP+I+NI*(J-1)+NI*NJ*(K-1)),
CC     &                    I=1,NI),J=1,NJ),K=1,NK)
              READ(IPRFIL,*,END=9020,ERR=9030) (GGVLN(I,IB),I=1,3)
CC              WRITE(2001,'(A,I3,A,1P,3E10.2)')
CC     &                    '(D2)-',IB,':',(GGVLN(I,IB),I=1,3)
            ENDIF
            NP=NP+NI*NJ*NK
 220      CONTINUE
          DO 230 L=1,IPRNP
CN          GGVNOW(L)=MAX(GGVNOW(L),PLOWER)
            GGVNOW(L)=MAX(GGVNOW(L),PLOWER*0.8D0)
 230      CONTINUE
          GOTO 200

 240      CONTINUE
          IF (IPRIT.EQ.1) THEN
            PRTOLD=PRTNOW
            DO 250 L=1,IPRNP
              GGVOLD(L)=GGVNOW(L)
 250        CONTINUE
            DO 251 L=1,IPRNB
              GGVLO(1,L)=GGVLN(1,L)
              GGVLO(2,L)=GGVLN(2,L)
              GGVLO(3,L)=GGVLN(3,L)
 251        CONTINUE
          ENDIF
        ENDIF
        IF (NPROCS.NE.1) THEN
          CALL VF_P1BCSI(IPRIT ,    1,0)
          CALL VF_P1BCSD(PRTOLD,    1,0)
          CALL VF_P1BCSD(PRTNOW,    1,0)
          CALL VF_P1BCSD(GGVOLD,IPRNP,0)
          CALL VF_P1BCSD(GGVNOW,IPRNP,0)
          CALL VF_P1BCSD(GGVLO,IPRNB,0)
          CALL VF_P1BCSD(GGVLN,IPRNB,0)
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
C----------------------------------------------------for MG/2FC coupling
C     CALL VF_A2ERR('VF_IP1INP','CAN NOT OPEN (data.poro).')
      CALL VF_A2ERR('VF_IP1INP','CAN NOT OPEN ('
     &                  //TRIM(MGNAME(MGRANK+1))//'.poro).')
C----------------------------------------------------for MG/2FC coupling
      GOTO 9999

 9020 CONTINUE
C----------------------------------------------------for MG/2FC coupling
C     CALL VF_A2ERR('VF_IP1INP','END OF FILE (data.poro).')
      CALL VF_A2ERR('VF_IP1INP','END OF FILE ('
     &                 //TRIM(MGNAME(MGRANK+1))//'.poro).')
C----------------------------------------------------for MG/2FC coupling
      GOTO 9999

 9030 CONTINUE
C----------------------------------------------------for MG/2FC coupling
C     CALL VF_A2ERR('VF_IP1INP','I/O ERROR (data.poro).')
      CALL VF_A2ERR('VF_IP1INP','I/O ERROR ('
     &               //TRIM(MGNAME(MGRANK+1))//'.poro).')
C----------------------------------------------------for MG/2FC coupling
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-PORO : IN : INITIAL')
 9520 FORMAT( ' ','>> FILE-PORO : IN : IPRIT=',I6,' : PTIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
