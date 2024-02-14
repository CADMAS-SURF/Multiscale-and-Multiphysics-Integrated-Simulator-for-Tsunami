      SUBROUTINE INPUTC
C----------------------------------------
C     解析条件の読込み
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_FLUID
      USE M_MODEL
      USE M_OUTPUT
      USE M_FILEIN,ONLY: OFF_INTERVAL,OFF_START,ONLINE,OFFLINE
     $                  ,NOCAL,OFF_OUTPUT
      USE M_WIND
      USE M_SUBGRID,ONLY: MAXDIV_X,MAXDIV_Y
C
      IMPLICIT NONE
C
      REAL(8),PARAMETER::EPS=1.0D-6
      REAL(8)::START,END,C,RESTART_TIME
      INTEGER::MAXSTEP
      CHARACTER(8)::MODE='',RESTART='',MODEL=''
      LOGICAL::CALC=.false.
C
      INTEGER::N,IERR
C
C
C ... FOR input.dat
      NAMELIST /TIME_DATA/ START,END,DT,C,DTMAX,DTMIN,RESTART_TIME,
     $   MAXSTEP,MODE,RESTART
C
      NAMELIST /FLUID_DATA/ RHO
C
      NAMELIST /WIND_DATA/ CALC,RHOA,WIND_U,WIND_V,WIND_FILE
C
      CHARACTER(8):: RAND_MODEL
      INTEGER:: RAND_SEED
      REAL(8):: RAND_MANNING,XRAND
      NAMELIST /DRIFT_DATA/ CM,LNGDIV,MODEL,MAXDIV_L,MAXDIV_B
     $                     ,RAND_MODEL,RAND_SEED,RAND_MANNING
C
      NAMELIST /OUTPUT_DATA/ DRIFT_START,DRIFT_INTERVAL,RESTART_START,
     $   RESTART_INTERVAL,BLOCK_START,BLOCK_INTERVAL,
     $   STL_START,STL_INTERVAL
C
      NAMELIST /OFFLINE_DATA/ OFFLINE,NOCAL,OFF_INTERVAL,OFF_START
C
      NAMELIST /SUBGRID_DATA/ MAXDIV_X,MAXDIV_Y
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ... SET DEFAULT VALUE
      IF( L_RAND==0 ) RAND_MODEL='OFF'
      IF( L_RAND==1 ) RAND_MODEL='ON'
      RAND_SEED=I_RAND_SEED(1)
      RAND_MANNING=AMNG_RAND
C
C ... ファイルオープン
      OPEN (11,FILE='./drift/input.dat',
     &                             STATUS='OLD',FORM='FORMATTED',ERR=90)
C
      REWIND(11)
      READ(11,TIME_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /TIME_DATA/'
         CALL ERRMSG('INPUTC',-4)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /TIME_DATA/ IS NOT FOUND'
         WRITE(*,*) 'THIS ITEM CANNOT BE OMITTED'
         CALL ERRMSG('INPUTC',-5)
      ENDIF
C
      REWIND(11)
      READ(11,FLUID_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /FLUID_DATA/'
         CALL ERRMSG('INPUTC',-6)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /FLUID_DATA/ IS NOT FOUND'
         WRITE(*,*) 'DEFAULT VALUE ARE APPLIED (RHO=1.E+3)'
         CALL ERRMSG('INPUTC',7)
         RHO = 1.D3
      ENDIF
C     
      REWIND(11)
      READ(11,WIND_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /WIND_DATA/'
         CALL ERRMSG('INPUTC',-8)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /WIND_DATA/ IS NOT FOUND'
         WRITE(*,*) 'WIND IS NOT CONSIDERED'
         CALL ERRMSG('INPUTC',9)
         CALC = .false.
      ENDIF
C
      REWIND(11)
      READ(11,SUBGRID_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /SUBGRID_DATA/'
         CALL ERRMSG('INPUTC',-18)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /SUBGRID_DATA/ IS NOT FOUND'
         WRITE(*,*) 'DEFAULT VALUES ARE USED'
         CALL ERRMSG('INPUTC',19)
      ENDIF
C
      REWIND(11)
      READ(11,DRIFT_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /DRIFT_DATA/'
         CALL ERRMSG('INPUTC',-10)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /DRIFT_DATA/ IS NOT FOUND'
         WRITE(*,*) 'THIS ITEM CANNOT BE OMITTED'
         CALL ERRMSG('INPUTC',-11)
      ENDIF
C
      REWIND(11)
      READ(11,OUTPUT_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /OUTPUT_DATA/'
         CALL ERRMSG('INPUTC',-12)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /OUTPUT_DATA/ IS NOT FOUND'
         WRITE(*,*) 'THIS ITEM CANNOT BE OMITTED'
         CALL ERRMSG('INPUTC',-13)
      ENDIF
C
      REWIND(11)
      READ(11,OFFLINE_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /OFFLINE_DATA/'
         CALL ERRMSG('INPUTC',-14)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /OFFLINE_DATA/ IS NOT FOUND'
         WRITE(*,*) 'THIS FEATURE IS NOT APPLIED'
         CALL ERRMSG('INPUTC',15)
      ENDIF
C
      CLOSE(11)
C
C----------------------------------------
C     TIME_DATAの処理
C----------------------------------------
      TSTART = START
      TEND   = END
      MAXSTP = MAXSTEP
      CSAFE  = C
      IF( MODE.EQ.'CONSTANT' ) THEN
         IDT = 0
      ELSEIF( MODE.EQ.'AUTO    ' ) THEN
         IDT = 1
      ELSE
         WRITE(*,*) 'ERROR: MODE MUST BE CONSTANT OR AUTO'
         WRITE(*,*) '       CHECK input.dat'
         CALL ERRMSG('INPUTC',-1)
      ENDIF
      IF( RESTART.EQ.'NO      ' ) THEN
         IREST = 0
      ELSEIF( RESTART.EQ.'YES     ' ) THEN
         IREST = 1
         TSTART = RESTART_TIME
      ELSE
         WRITE(*,*) 'ERROR: RESTART MUST BE YES OR NO'
         WRITE(*,*) '       CHECK input.dat'
         CALL ERRMSG('INPUTC',-2)
      ENDIF
C
C----------------------------------------
C     WIND_DATAの処理
C----------------------------------------
      WIN_CAL=CALC
      DO N=1,MAXPE
         IF( WIND_FILE(N)(1:1).EQ.' ' ) WIN_FIX(N)=.true.
      ENDDO
C
C----------------------------------------
C     DRIFT_DATAの処理
C----------------------------------------
      IF( trim(MODEL)=='H21' ) SINK_MODEL=0
      IF( RAND_MODEL=='OFF' ) L_RAND=0
      IF( RAND_MODEL=='ON'  ) L_RAND=1
      AMNG_RAND=RAND_MANNING
      IF( MAXDIV_L.LE.1.OR.MAXDIV_B.LE.1 ) THEN
         WRITE(*,*) 'MAXDIV_L,MAXDIV_B >= 2'
         CALL ERRMSG('INPUTC',-17)
      ENDIF
C
C ... INITIALIZE RANDOM SEED
      IF( L_RAND==1 ) THEN
         CALL RANDOM_SEED(SIZE=SEEDSIZE)
         IF( SEEDSIZE>MAXSEED ) THEN
            WRITE(*,*) 'SEEDSIZE > MAXSEED'
            WRITE(*,*) 'PLEASE MODIFY MAXSEED IN M_MODEL >=',SEEDSIZE
            CALL ERRMSG('INPUTC',-16)
         ENDIF
C
         I_RAND_SEED(1:SEEDSIZE)=RAND_SEED
         CALL RANDOM_SEED(PUT=I_RAND_SEED)
C
C        CHANGE SEED LARGE VALUE
         DO N=1,SEEDSIZE
            CALL RANDOM_NUMBER(XRAND)
            I_RAND_SEED(N)=INT(XRAND*DBLE(HUGE(1)))
         ENDDO
         CALL RANDOM_SEED(PUT=I_RAND_SEED)
      ENDIF
C
C----------------------------------------
C     OFFLINE_DATAの処理
C----------------------------------------
      ONLINE=.NOT.OFFLINE
      IF( OFF_INTERVAL.GT.0.0D0 ) OFF_OUTPUT=.true.
      IF( OFFLINE ) OFF_OUTPUT=.false.
C      OFF_START=MIN(OFF_START,START-OFF_INTERVAL)
C
C----------------------------------------
C     OUTPUT_DATAの処理
C----------------------------------------
      DRIFT_START  =MAX(DRIFT_START  ,TSTART)
      RESTART_START=MAX(RESTART_START,TSTART)
      BLOCK_START  =MAX(BLOCK_START  ,TSTART)
      STL_START    =MAX(STL_START    ,TSTART)
      STL_INTERVAL =MAX(STL_INTERVAL ,1.0D0+EPS)
C
C
      WRITE(IFL,TIME_DATA)
      WRITE(IFL,FLUID_DATA)
      WRITE(IFL,WIND_DATA)
      WRITE(IFL,DRIFT_DATA)
      WRITE(IFL,OUTPUT_DATA)
      WRITE(IFL,OFFLINE_DATA)
C
      RETURN
C
   90 CONTINUE
      WRITE(*,*) 'ERROR: cannot open input.dat'
      CALL ERRMSG('INPUTC',-3)
      END
