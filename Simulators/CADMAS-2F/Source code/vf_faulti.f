      SUBROUTINE VF_FAULTI(TIME,TIMESB,TIM1)

CD=== 概要 ===========================================================

CDT   VF_FAULTI:断層パラメータから水位変動量を計算するための初期化処理

C==== 宣言 ===========================================================

      USE MOD_FAULT,ONLY: SET_PARAM_FAULT,D2R,
     $                    ISIZ_PARAM,NFLT,NFLTNOW,FPARAM,
     $                    XOR,YOR,ISYSTEM,JSYSTEM,LB2LB

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)
C      IMPLICIT NONE

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
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
      OPEN(MFILBT,FILE='fault_cad.txt',FORM='FORMATTED',STATUS='OLD',
     $     ERR=900)
C
C ... ファイルの行数をカウント
      WRITE(ILPFIL,*) 'READING fault.txt ...'
      N=0
      DO
        READ(MFILBT,*,END=10,ERR=910) FL,FW,H,STR,DIP,SLIP,DIS,B,L,TIMEF
C
         IF( DIS.EQ.0.0D0 ) CYCLE
         N=N+1
      ENDDO
   10 CONTINUE
      NFLT=N
      WRITE(ILPFIL,*) '   NUMBER OF FAULT PARAMETER = ',NFLT
      WRITE(ILPFIL,*) '   (IGNORE THE DATA OF DISPLACEMENT=0.0)'
C
      ALLOCATE(FPARAM(ISIZ_PARAM,NFLT),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL VF_A2ERR('VF_FAULTI','CANNOT ALLOCATE FPARAM.')
      ENDIF
      FPARAM(:,:)=0.D0
C
      REWIND MFILBT
      N=0
      BMIN=1.D9
      BMAX=-1.D9
      LMIN=1.D9
      LMAX=-1.D9
      DO
        READ(MFILBT,*,END=20,ERR=910) FL,FW,H,STR,DIP,SLIP,DIS,B,L,TIMEF
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
      IF(N.NE.NFLT) CALL VF_A2ERR('VF_FAULTI','MISMATCH.')
      WRITE(ILPFIL,*) 'END OF READING fault.txt'
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
      CLOSE(MFILBT)
C
C ... 断層パラメータの並べ替え(時刻順)
      CALL VF_HSORT(FPARAM,ISIZ_PARAM,NFLT,10,ILPFIL)
C
C
C     リスタート時はリスタート時刻以前のデータを読み飛ばすためにループ処理を行う
      DO NFLTNOW=1,NFLT
         TIMESB=FPARAM(10,NFLTNOW)
         write(ILPFIL,*) '### FAULT DATA: TIMESB=',TIMESB
C
C ...... 0.0秒で水位変動が設定されたとき
         IF(TIME.EQ.TIMESB.AND.TIMESB.EQ.0.0D0) THEN
            TIM1=TIMESB
            GOTO 30
         ELSEIF(TIM1.LT.TIMESB) THEN
            GOTO 30
         ENDIF
      ENDDO
      write(ILPFIL,*) '### FAULT DATA: END OF DATA'
      TIMESB=1.D+99
C
   30 CONTINUE
C
      RETURN
C
  900 CONTINUE
      CALL VF_A2ERR('VF_FAULTI','CANNOT OPEN fault_cad.txt.')
C
  910 CONTINUE
      WRITE(ILPFIL,*) 'LINE NUMBER =',NFLT+1
      CALL VF_A2ERR('VF_FAULTI','CANNOT READ fault_cad.txt.')
      END
