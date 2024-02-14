      SUBROUTINE MGETC(CVAL,ISIZ,NDAT,MXDAT)
C======================================================================
C     入力データから複数の文字型データを読み込む
C
C     CVAL:  文字型配列
C     ISIZ:  文字列の最大長
C     NDAT:  読み込んだデータの数
C     MXDAT: 最大データ数(CVALの配列サイズ)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      INTEGER,INTENT(INOUT)::ISIZ,NDAT,MXDAT
      CHARACTER(*),INTENT(INOUT)::CVAL(MXDAT)
C
      INTEGER::IE,IERR,IS,N
C
C
C----------------------------------------------------------------------
C     (1) '='を読み込む
C----------------------------------------------------------------------
C
      CALL GET1(IS,IE,IERR)
      IF( IERR.GT.0 ) GO TO 900
C
      IF( CLINE(IS:IE) .NE. '=' ) THEN
         CALL ERRMSG('MGETC',6350)
         WRITE(LP,*) 'INPUT DATA FORMAT IS INCORRECT'
         WRITE(LP,*) 'EQUAL SYMBOL IS NEEDED',
     $               ' BETWEEN VARIABLE AND VALUES'
         WRITE(LP,*) 'LINE=',CLINE
         CALL ABORT1('')
      END IF
C
C
C----------------------------------------------------------------------
C     (2) "値"(右辺値)を読み込む
C----------------------------------------------------------------------
C
      CALL GET1(IS,IE,IERR)
      IF( IERR.GT.0 ) GO TO 900
C
C ... 値が複数ある場合
      IF( CLINE(IS:IE) .EQ. '(' ) THEN
         DO 100 N=1,100000
            CALL GET1(IS,IE,IERR)
            IF( IERR.GT.0 ) GO TO 900
C
            IF( CLINE(IS:IE) .EQ. ')' ) THEN
               IF( N .EQ. 1 ) THEN
                  CALL ERRMSG('MGETC',6351)
                  WRITE(LP,*) 'THERE IS NO DATA'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               ELSE
                  NDAT = N-1
                  GO TO 200
               END IF
            ELSE
               IF( I1DD.GE.0 .AND. N.GT.MXDAT ) THEN
                  CALL ERRMSG('MGETC',6352)
                  WRITE(LP,*) 'THE NUMBER OF DATA MAY ',
     $                        'NOT BE OVER ',MXDAT,' HERE'
                  WRITE(LP,*) 'ERROR DATA=',CLINE(IS:IE)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               IF( I1DD.GE.0 .AND. IE-IS+1.GT.ISIZ ) THEN
                  CALL ERRMSG('MGETC',6353)
                  WRITE(LP,*) 'STRING LENGTH MAY ',
     $                        'NOT BE OVER ',ISIZ,' HERE'
                  WRITE(LP,*) 'ERROR DATA=',CLINE(IS:IE)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               CVAL(N) = CNUL(1:ISIZ)
               CVAL(N) = CLINE(IS:IE)
               IF(I1DD.LT.0) GO TO 10
            END IF
  100    CONTINUE
  200    CONTINUE
C
C ... 値が1つだけの場合
      ELSE
         IF( I1DD.GE.0 .AND. IE-IS+1.GT.ISIZ ) THEN
            CALL ERRMSG('MGETC',6354)
            WRITE(LP,*) 'STRING LENGTH MAY ',
     $                  'NOT BE OVER ',ISIZ,' HERE'
            WRITE(LP,*) 'ERROR DATA=',CLINE(IS:IE)
            WRITE(LP,*) 'LINE=',CLINE
            CALL ABORT1('')
         END IF
         CVAL(1) = CNUL(1:ISIZ)
         CVAL(1) = CLINE(IS:IE)
         NDAT = 1
      END IF
C
c      if( ndat.gt.1 ) write(*,*) 'debug:mgetc:ndat=',ndat
c      write(*,*) 'debug:mgetc:value=',(cval(i)(1:isiz),i=1,ndat)
   10 CONTINUE
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('MGETC',6355)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
