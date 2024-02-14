      SUBROUTINE INPUTF
C----------------------------------------
C     フェンス状構造物の条件の読込み
C----------------------------------------
      USE M_FENCE,ONLY:NFC,XFC1,YFC1,XFC2,YFC2,ZFC,DFC_LIMIT,EFC_LIMIT,
     $                 LFC,KFC,ALLOCATE_FENCE
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      INTEGER::M,N,NN,KFCW,IERR
      REAL(8)::XFC1W,YFC1W,XFC2W,YFC2W,ZFCW,DFCW,EFCW,DX,DY
      REAL(8)::XFMIN=1.0D10,XFMAX=-1.0D10
      REAL(8)::YFMIN=1.0D10,YFMAX=-1.0D10
      CHARACTER(1)::CH
C
C
      OPEN (15,FILE='./drift/fence.dat',
     &                             STATUS='OLD',FORM='FORMATTED',ERR=90)
C
C ... フェンスの数をカウント
      NFC = 0
      DO N=1,99999999
         READ(15,'(A1)',END=20) CH
         IF( CH.NE.'#' ) THEN
            BACKSPACE(15)
            READ(15,*) M,XFC1W,YFC1W,XFC2W,YFC2W,ZFCW,KFCW,
     $                    NN,DFCW,EFCW
            NFC=NFC+NN
C
            IF(NN.LE.0) THEN
               WRITE(*,*) 'NUMBER OF DIVISION MUST BE >= 0'
               WRITE(*,*) 'AT NO=',M,' IN FENCE.DAT'
               CALL ERRMSG('INPUTF',-1)
            ENDIF
         ENDIF
      ENDDO
   20 CONTINUE
      WRITE(IFL,*)
      WRITE(IFL,*) 'NFC=',NFC
      REWIND(15)
      CALL ALLOCATE_FENCE(NFC,IERR)
      IF( IERR.GT.0 ) GOTO 91
C
      NFC=0
      DO N=1,99999999
C ...... ファイルの読み込み (#で始まる行は読み飛ばす)
         READ(15,'(A1)',END=10) CH
         IF( CH.NE.'#' ) THEN
C
            BACKSPACE(15)
            READ(15,*) M,XFC1W,YFC1W,XFC2W,YFC2W,ZFCW,KFCW,
     $                 NN,DFCW,EFCW
C
            IF(KFCW.NE.0.AND.KFCW.NE.1) THEN
               WRITE(*,*) 'TYPE MUST BE 0 OR 1'
               WRITE(*,*) 'AT NO=',M,' IN FENCE.DAT'
               CALL ERRMSG('INPUTF',-2)
            ENDIF
C
            XFMIN = MIN(XFMIN,XFC1W)
            XFMAX = MAX(XFMAX,XFC1W)
            YFMIN = MIN(YFMIN,YFC1W)
            YFMAX = MAX(YFMAX,YFC1W)
            XFMIN = MIN(XFMIN,XFC2W)
            XFMAX = MAX(XFMAX,XFC2W)
            YFMIN = MIN(YFMIN,YFC2W)
            YFMAX = MAX(YFMAX,YFC2W)
C
C ......... フェンスの分割
            IF( NN.GE.1 ) THEN
               DX=(XFC2W-XFC1W)/DBLE(NN)
               DY=(YFC2W-YFC1W)/DBLE(NN)
            ENDIF
C
            DO M=1,NN
               NFC=NFC+1
               XFC1(NFC)=XFC1W+DBLE(M-1)*DX
               YFC1(NFC)=YFC1W+DBLE(M-1)*DY
               XFC2(NFC)=XFC1W+DBLE(M)*DX
               YFC2(NFC)=YFC1W+DBLE(M)*DY
               ZFC(NFC)=ZFCW
               KFC(NFC)=KFCW
               DFC_LIMIT(NFC)=DFCW
               EFC_LIMIT(NFC)=EFCW
C
CDEBUG               write(99,*) xfc1(nfc),yfc1(nfc)
CDEBUG               write(99,*) xfc2(nfc),yfc2(nfc)
CDEBUG               write(99,*)
C
               LFC(NFC)=0
            ENDDO
C
         ENDIF
      ENDDO
   10 CONTINUE
C
      CLOSE(15)
C
C ... 入力データの簡易情報出力
      WRITE(IFL,*) 'INPUT FENCE DATA END'
      WRITE(IFL,*) 'NUMBER OF FENCE IS ',NFC
      WRITE(IFL,*) 'X-RANGE IS ',XFMIN,' - ',XFMAX
      WRITE(IFL,*) 'Y-RANGE IS ',YFMIN,' - ',YFMAX
C
      RETURN
C
   90 CONTINUE
      WRITE(*,*) 'THERE IS NO FENCE DATA.'
      NFC=0
      CALL ALLOCATE_FENCE(1,IERR)
      IF( IERR.GT.0 ) GOTO 91
      RETURN
C
   91 CONTINUE
      WRITE(*,*) 'ERROR END:STOC-DM'
      CALL ERRMSG('INPUTF',-3)
      END
