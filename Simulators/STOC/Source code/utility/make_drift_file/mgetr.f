      SUBROUTINE MGETR(RVAL,NDAT,MXDAT)
C======================================================================
C     入力データから複数の実数型データを読み込む
C
C     RVAL:  実数型配列
C     NDAT:  読み込んだデータの数
C     MXDAT: 最大データ数(RVALの配列サイズ)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      INTEGER,INTENT(INOUT)::NDAT,MXDAT
      REAL(8),INTENT(INOUT)::RVAL(MXDAT)
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
         CALL ERRMSG('MGETR',6380)
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
                  CALL ERRMSG('MGETR',6381)
                  WRITE(LP,*) 'THERE IS NO DATA'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               ELSE
                  NDAT = N-1
                  GO TO 200
               END IF
            ELSE
               IF( N .GT. MXDAT ) THEN
                  CALL ERRMSG('MGETR',6382)
                  WRITE(LP,*) 'THE NUMBER OF DATA MAY ',
     $                        'NOT BE OVER ',MXDAT,' HERE'
                  WRITE(LP,*) 'ERROR DATA=',CLINE(IS:IE)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               READ(CLINE(IS:IE),'(BN,F80.0)',ERR=910) RVAL(N)
            END IF
  100    CONTINUE
  200    CONTINUE
C
C ... 値が1つだけの場合
      ELSE
         READ(CLINE(IS:IE),'(BN,F80.0)',ERR=910) RVAL(1)
         NDAT = 1
      END IF
C
c      if( ndat.gt.1 ) write(*,*) 'debug:mgetr:ndat=',ndat
c      write(*,*) 'debug:mgetr:value=',(rval(i),i=1,ndat)
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('MGETR',6383)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
C
  910 CONTINUE
      CALL ERRMSG('MGETR',6384)
      WRITE(LP,*) 'REAL DATA TYPE IS EXPECTED HERE'
      WRITE(LP,*) 'DATA=',CLINE(IS:IE)
      WRITE(LP,*) 'LINE=',CLINE
      CALL ABORT1('')
      END
