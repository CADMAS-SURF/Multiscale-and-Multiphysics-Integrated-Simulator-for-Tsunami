      SUBROUTINE INBOUN(icoord,isystem,lc_deg)
C======================================================================
C     境界条件を読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'CONNEC.h'
C
      CHARACTER(23)::CTMP
C
      INTEGER:: ICOORD,ISYSTEM
      REAL(8)::LC_DEG
      INTEGER::LL=-1
C
      INTEGER::IE,IERR,IS
      INTEGER::N
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 300
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRID-SYSTEM' ) THEN
            CALL GETC(CTMP,7)
            IF( CTMP.EQ.'TOKYO  ' ) THEN
               ISYSTEM = 1
            ELSE IF( CTMP.EQ.'JGD2000' ) THEN
               ISYSTEM = 2
            ELSE IF( CTMP.EQ.'WGS84  ' ) THEN
               ISYSTEM = 3
            ELSE
               CALL ERRMSG('INBOUN',6538)
               WRITE(LP,*)'VALUE MUST BE TOKYO OR JGD2000 OR WGS84'
               WRITE(LP,*) 'VARIABLE=GRID-SYSTEM'
               WRITE(LP,*) 'VALUE=',trim(CTMP)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'COORDINATE' ) THEN
            CALL GETC(CTMP,23)
            IF( CTMP.EQ.'JAPAN-PLANE-RECTANGULAR' ) THEN
               LL = 1
            ELSE IF( CTMP.EQ.'UTM                    ' ) THEN
               LL = 0
            ELSE IF( CTMP.EQ.'LONGITUDE-LATITUDE     ' ) THEN
               LL = 2
            ELSE
               CALL ERRMSG('INBOUN',6539)
               WRITE(LP,*) 'VALUE MUST BE JAPAN-PLANE-RECTANGULAR OR'
               WRITE(LP,*) '   UTM OR LONGITUDE-LATITUDE'
               WRITE(LP,*) 'VARIABLE=COORDINATE'
               WRITE(LP,*) 'VALUE=',trim(CTMP)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RECTANGULAR-ZONE' ) THEN
            CALL GETI(ICOORD)
            IF( ICOORD.LT.1.OR.ICOORD.GT.19 ) THEN
               CALL ERRMSG('INBOUN',6540)
               WRITE(LP,*)'VALUE MUST BE 1-19'
               WRITE(LP,*) 'VARIABLE=RECTANGULAR-ZONE'
               WRITE(LP,*) 'VALUE=',ICOORD
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'UTM-CENTER' ) THEN
            CALL GETR(LC_DEG)
C
         ELSE
C            CALL ERRMSG('INBOUN',6541)
C            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
C            CALL ABORT1('')
         END IF
  100 CONTINUE
  300 CONTINUE
C
C
C ...... (a) UTMの場合
         IF( LL.EQ.0 ) THEN
            ICOORD=0
C
            IF(LC_DEG.LT.-999.D0) THEN
               CALL ERRMSG('INBOUN',6542)
               WRITE(LP,*) 'UTM-CENTER IS NOT DEFINED'
               CALL ABORT1('')
            ENDIF
C
C ...... (b) 19座標系の場合
         ELSEIF( LL.EQ.1 ) THEN
            IF( ICOORD.LE.0.OR.ICOORD.GT.19 ) THEN
               CALL ERRMSG('INBOUN',6543)
               WRITE(LP,*) 'RCTANGULAR-ZONE(1-19) IS NOT DEFINED'
               CALL ABORT1('')
            ENDIF
C
C
C ...... (c) 緯度経度座標系の場合
         ELSEIF( LL.EQ.2 ) THEN
            ICOORD=-1
C
C ...... (d) その他
         ELSE
            CALL ERRMSG('INBOUN',6544)
            WRITE(LP,*) 'COORDINATE DATA IS NOT DEFINED.'
            CALL ABORT1('')
         ENDIF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INBOUN',6545)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
C
      END
