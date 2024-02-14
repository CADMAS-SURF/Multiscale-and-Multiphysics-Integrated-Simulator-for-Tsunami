      SUBROUTINE FAULTI(TIME,TIMESB,TIM1)
C----------------------------------------------------------------------
C     断層パラメータから水位変動量を計算するための初期化処理
C----------------------------------------------------------------------
      USE MOD_FAULT,ONLY: SET_PARAM_FAULT,D2R,
     $                    ISIZ_PARAM,NFLT,NFLTNOW,FPARAM,
     $                    XOR,YOR,ISYSTEM,JSYSTEM,LB2LB
      IMPLICIT NONE
      INCLUDE 'FILE.h'
C
      REAL(8):: TIME,TIMESB,TIM1
      REAL(8):: FL,FW,H,STR,DIP,SLIP,DIS,B,L,TIMEF
      REAL(8):: BMIN,BMAX,LMIN,LMAX
      INTEGER:: N,IERR
C
C
C ... 計算パラメータの初期化
      CALL SET_PARAM_FAULT
C
C ... 断層パラメータの読み込みと並べ替え
      OPEN(IFLSB,FILE='fault.txt',FORM='FORMATTED',STATUS='OLD',ERR=900)
C
C ... ファイルの行数をカウント
      WRITE(LP,*) 'READING fault.txt ...'
      N=0
      DO
         READ(IFLSB,*,END=10,ERR=910) FL,FW,H,STR,DIP,SLIP,DIS,B,L,TIMEF
C
         IF( DIS.EQ.0.0D0 ) CYCLE
         N=N+1
      ENDDO
   10 CONTINUE
      NFLT=N
      WRITE(LP,*) '   NUMBER OF FAULT PARAMETER = ',NFLT
      WRITE(LP,*) '   (IGNORE THE DATA OF DISPLACEMENT=0.0)'
C
      ALLOCATE(FPARAM(ISIZ_PARAM,NFLT),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('FAULTI',6920)
         WRITE(LP,*) 'CANNOT ALLOCATE FPARAM'
         CALL ABORT1('')
      ENDIF
      FPARAM(:,:)=0.D0
C
      REWIND IFLSB
      N=0
      BMIN=1.D9
      BMAX=-1.D9
      LMIN=1.D9
      LMAX=-1.D9
      DO
         READ(IFLSB,*,END=20,ERR=910) FL,FW,H,STR,DIP,SLIP,DIS,B,L,TIMEF
C
         IF( DIS.EQ.0.0D0 ) CYCLE
         N=N+1
C
         FPARAM(1,N)=FL
         FPARAM(2,N)=FW
         FPARAM(3,N)=H
         FPARAM(4,N)=STR*D2R
         FPARAM(5,N)=DIP*D2R
         FPARAM(6,N)=SLIP*D2R
         FPARAM(7,N)=DIS
         FPARAM(8,N)=B*D2R
         FPARAM(9,N)=L*D2R
         FPARAM(10,N)=TIMEF
         BMIN=MIN(BMIN,B)
         BMAX=MAX(BMAX,B)
         LMIN=MIN(LMIN,L)
         LMAX=MAX(LMAX,L)
      ENDDO
   20 CONTINUE
      IF(N.NE.NFLT) THEN
         CALL ERRMSG('FAULTI',6921)
         WRITE(LP,*) 'N=',N,' NFLT=',NFLT
         CALL ABORT1('')
      ENDIF
      WRITE(LP,*) 'END OF READING fault.txt'
      XOR=0.5D0*(LMIN+LMAX)
      YOR=0.5D0*(BMIN+BMAX)
      WRITE(LP,*) 'XOR,YOR=',XOR,YOR
      XOR=XOR*D2R
      YOR=YOR*D2R
      IF( ISYSTEM.NE.JSYSTEM ) THEN
         CALL LB2LB(XOR,YOR,L,B,ISYSTEM,JSYSTEM)
         XOR=L
         YOR=B
      ENDIF
      CLOSE(IFLSB)
C
C ... 断層パラメータの並べ替え(時刻順)
      CALL HSORT(FPARAM,ISIZ_PARAM,NFLT,10,LP)
C
C
C     リスタート時はリスタート時刻以前のデータを読み飛ばすためにループ処理を行う
      DO NFLTNOW=1,NFLT
         TIMESB=FPARAM(10,NFLTNOW)
         write(LP,*) '### FAULT DATA: TIMESB=',TIMESB
C
C ...... 0.0秒で水位変動が設定されたとき
         IF(TIME.EQ.TIMESB.AND.TIMESB.EQ.0.0D0) THEN
            TIM1=TIMESB
            GOTO 30
         ELSEIF(TIME.LT.TIMESB) THEN
            GOTO 30
         ENDIF
      ENDDO
      write(LP,*) '### FAULT DATA: END OF DATA'
      TIMESB=1.D+99
C
   30 CONTINUE
C
      RETURN
C
  900 CONTINUE
      CALL ERRMSG('FAULTI',6922)
      WRITE(LP,*) 'ERROR: CANNOT OPEN fault.txt'
      CALL ABORT1('')
C
  910 CONTINUE
      CALL ERRMSG('FAULTI',6923)
      WRITE(LP,*) 'ERROR: CANNOT READ fault.txt'
      WRITE(LP,*) '       LINE NUMBER =',NFLT+1
      CALL ABORT1('')
      END
