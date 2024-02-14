      SUBROUTINE VF_OT1INI()

CD=== 概要 ===========================================================

CDT   VF_OT1INI:出力対象を時系列ファイルに出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 局所変数 --
      CHARACTER*12 TEXT1,TEXT2

C==== 実行 ===========================================================

CD    -- 出力指定がないかマスターでなければ抜ける --
      IF (ITRTYP.EQ.0) GOTO 9000
      IF (MYRANK.NE.0) GOTO 9000

CD    -- 時系列ファイルのオープンとメッセージの出力 --
      ITRFIL=0
      OPEN(MFILTR,ERR=9010,
     &     FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.tran',
     &     STATUS='NEW',FORM='FORMATTED' )
      ITRFIL=MFILTR

CD    -- 出力対象の出力 --
      DO 100 L=1,ITRNUM
        IF     (ITRPRM(1,L).EQ. 0) THEN
          TEXT1='W-LEVEL'
        ELSEIF (ITRPRM(1,L).EQ. 1) THEN
          TEXT1='POINT'
        ELSEIF (ITRPRM(1,L).EQ. 2) THEN
          TEXT1='MIN'
        ELSEIF (ITRPRM(1,L).EQ. 3) THEN
          TEXT1='MAX'
        ELSEIF (ITRPRM(1,L).EQ. 4) THEN
          TEXT1='AV'
        ELSEIF (ITRPRM(1,L).EQ. 5) THEN
          TEXT1='INT'
        ELSEIF (ITRPRM(1,L).EQ.11) THEN
          TEXT1='FORCE'
        ELSE
          CALL VF_A2ERR('VF_OT1INI','P.G ERROR(1).')
        ENDIF
        IF     (ITRPRM(2,L).EQ.0) THEN
          IF (ITRPRM(3,L).GE. 0) THEN
            TEXT2='------'
          ELSE
            TEXT2='ANS'
          ENDIF
        ELSEIF (ITRPRM(2,L).EQ. 1) THEN
          TEXT2='U'
        ELSEIF (ITRPRM(2,L).EQ. 2) THEN
          TEXT2='V'
        ELSEIF (ITRPRM(2,L).EQ. 3) THEN
          TEXT2='W'
        ELSEIF (ITRPRM(2,L).EQ. 4) THEN
          TEXT2='P'
        ELSEIF (ITRPRM(2,L).EQ. 5) THEN
          TEXT2='F'
        ELSEIF (ITRPRM(2,L).EQ. 6) THEN
          TEXT2='K'
        ELSEIF (ITRPRM(2,L).EQ. 7) THEN
          TEXT2='E'
        ELSEIF (ITRPRM(2,L).EQ. 8) THEN
          TEXT2='T'
        ELSEIF (ITRPRM(2,L).LE.-1) THEN
          TEXT2='C'
        ELSEIF (ITRPRM(2,L).EQ.11) THEN
          TEXT2='X-'
        ELSEIF (ITRPRM(2,L).EQ.12) THEN
          TEXT2='X+'
        ELSEIF (ITRPRM(2,L).EQ.13) THEN
          TEXT2='Y-'
        ELSEIF (ITRPRM(2,L).EQ.14) THEN
          TEXT2='Y+'
        ELSEIF (ITRPRM(2,L).EQ.15) THEN
          TEXT2='Z-'
        ELSEIF (ITRPRM(2,L).EQ.16) THEN
          TEXT2='Z+'
        ELSEIF (ITRPRM(2,L).EQ.17) THEN
          TEXT2='VORT-X'
        ELSEIF (ITRPRM(2,L).EQ.18) THEN
          TEXT2='VORT-Y'
        ELSEIF (ITRPRM(2,L).EQ.19) THEN
          TEXT2='VORT-Z'
        ELSE
          CALL VF_A2ERR('VF_OT1INI','P.G ERROR(2).')
        ENDIF
        IF     (ITRPRM(3,L).GE. 0) THEN
          IF (ITRPRM(2,L).GE.0) THEN
            WRITE(ITRFIL,9510) L,TEXT1,TEXT2,
     &            ITRPRM(3,L)-1,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &            ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
          ELSE
            WRITE(ITRFIL,9540) L,TEXT1,TEXT2,-ITRPRM(2,L),
     &            ITRPRM(3,L)-1,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &            ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
          ENDIF
        ELSEIF (ITRPRM(3,L).EQ.-1) THEN
          WRITE(ITRFIL,9520) L,TEXT1,TEXT2,
     &          'X-'         ,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &          ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
        ELSEIF (ITRPRM(3,L).EQ.-2) THEN
          WRITE(ITRFIL,9520) L,TEXT1,TEXT2,
     &          'X+'         ,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &          ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
        ELSEIF (ITRPRM(3,L).EQ.-3) THEN
          WRITE(ITRFIL,9520) L,TEXT1,TEXT2,
     &          'Y-'         ,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &          ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
        ELSEIF (ITRPRM(3,L).EQ.-4) THEN
          WRITE(ITRFIL,9520) L,TEXT1,TEXT2,
     &          'Y+'         ,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &          ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
        ELSEIF (ITRPRM(3,L).EQ.-5) THEN
          WRITE(ITRFIL,9520) L,TEXT1,TEXT2,
     &          'SRC'        ,ITRPRM(4,L)-1,ITRPRM(5,L)-1,
     &          ITRPRM(6,L)-1,ITRPRM(7,L)-1,ITRPRM(8,L)-1
        ELSE
          CALL VF_A2ERR('VF_OT1INI','P.G ERROR(3).')
        ENDIF
 100  CONTINUE

CD    -- データ並びの出力 --
      WRITE(ITRFIL,9530) 'TIME',(L,L=1,ITRNUM)

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_OT1INI','CAN NOT OPEN (*****.tran).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(' ',I6,' ',A10,' ',A12  ,' ',I6,I6,I6,I6,I6,I6)
 9520 FORMAT(' ',I6,' ',A10,' ',A12  ,' ',A6,I6,I6,I6,I6,I6)
 9530 FORMAT( ' ',A15,10000(' ',I15:))
 9540 FORMAT(' ',I6,' ',A10,' ',A6,I6,' ',I6,I6,I6,I6,I6,I6)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
