      SUBROUTINE OUTOFFLNSD(HH,UU,VV,WW,IFLAG)
C======================================================================
C     オフライン土砂移動計算の出力制御を行う
C     IFLAG  = 0 : ファイルを開く
C            = 1 : ファイルに出力する
C            = 2 : ファイルを閉じる
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'SEDIMENT.h'
C
      REAL(8),INTENT(IN)::HH(MX,MY)
      REAL(8),INTENT(IN)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      INTEGER,INTENT(IN)::IFLAG
C
      INTEGER::I,J,K
      REAL(8),SAVE::TNEXTOUT
C
C----------------------------------------------------------------------
C     (1) ファイルを開く
C----------------------------------------------------------------------
      IF(IFLAG.EQ.0)THEN
C ...... ファイルのオープンとメッセージの出力
         call flnam('.osd')
         OPEN(IFLSD,FILE=trim(CFLNM),STATUS='NEW',
     $        FORM='UNFORMATTED',ERR=999)
         WRITE(LP,*) 'OPEN OFFLINE-SD-CALC FILE'
         WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
         WRITE(LP,*) 'FILE NUMBER=',IFLSD
C ...... 解析領域サイズを出力
         WRITE(IFLSD) MX,MY,MZ
C ...... 出力時刻をセット
         TNEXTOUT=TSOFFLN
      ENDIF
C
C----------------------------------------------------------------------
C     (2) ファイルに出力する
C----------------------------------------------------------------------
      IF(TIME.GT.TNEXTOUT-DT .AND. TIME.LT.TEOFFLN+DT)THEN
C ...... 水位および流速を出力
         WRITE(IFLSD) TIME
         WRITE(IFLSD) ((REAL(HH(I,J)),I=1,MX),J=1,MY)
         WRITE(IFLSD) (((REAL(UU(I,J,K)),I=1,MX),J=1,MY),K=2,MZM)
         WRITE(IFLSD) (((REAL(VV(I,J,K)),I=1,MX),J=1,MY),K=2,MZM)
         WRITE(IFLSD) (((REAL(WW(I,J,K)),I=1,MX),J=1,MY),K=2,MZM)
C ...... 次の出力時刻をセット
         TNEXTOUT=TNEXTOUT+DTOFFLN
      ENDIF
C
C----------------------------------------------------------------------
C     (3) ファイルを閉じる
C----------------------------------------------------------------------
      IF(IFLAG.EQ.2)THEN
C ...... ファイルのクローズとメッセージの出力
         CLOSE(IFLSD,STATUS='KEEP')
         WRITE(LP,*) 'CLOSE OFFLINE-SD-CALC FILE'
         WRITE(LP,*) 'FILE NUMBER=',IFLSD
      END IF
C
      RETURN
C
C
C ... ファイルオープンエラー
  999 CONTINUE
      CALL ERRMSG('OUTOFFLNSD',7160)
      WRITE(LP,*) 'FILE OPEN ERROR: OFFLINE-SD-CALC FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLSD
      CALL ABORT1('')
C
      END
