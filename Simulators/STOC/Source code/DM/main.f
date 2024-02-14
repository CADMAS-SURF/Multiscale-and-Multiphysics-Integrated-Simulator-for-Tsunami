      PROGRAM MAIN
      USE M_GRID,ONLY: MXAREA,MXNI,MXNJ,MXNK,MXNIJ,MXNIJK,
     $                    ALLOCATE_GRID
      USE M_FILEIN,ONLY : ONLINE,NOCAL,ALLOCATE_FILEIN
      USE M_FLUID,ONLY  : ALLOCATE_FLUID
      USE M_GEOM,ONLY   : ALLOCATE_GEOM
      USE M_TIME
      USE M_COM_STOC,ONLY: NSIZEALL,NSIZE,NRANK
      USE M_OUTPUT,ONLY:IFL
C
      use mod_comm,only: init_mpmd
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      INTEGER::IERR
      CHARACTER(7)::FILENAME
C
C
      call init_mpmd
C      
C<<<<< (START) STOC-DM VERSION  <<<<<<<
      CALL INIT_MPIENV(IERR)
C
      FILENAME(1:5)='FT16_'
      WRITE(FILENAME(6:7),'(i2.2)') NRANK
      OPEN(IFL,STATUS='UNKNOWN',FILE=FILENAME)
      WRITE(IFL,*) '+----------------------------------+'
      WRITE(IFL,*) '|  PROGRAM STOC(DM) VER.07.01.02v  |'
      WRITE(IFL,*) '+----------------------------------+'
      WRITE(IFL,*) ' MPI(DM) ==> NSIZEALL,NSIZE,NRANK= ',
     &                                              NSIZEALL,NSIZE,NRANK
C
C----------------------------------------
C     入力データの読込み
C----------------------------------------
      CALL INPUTC  ! 計算条件
C
      CALL COM_STOC0  !  STOC との初期通信
C
      IF( ONLINE.AND.NOCAL ) THEN ! NOCALのとき計算を行わず同期のみ行う。
         IFINISH = 0
         CALL ALLOCATE_GRID(MXAREA,MXNI,MXNJ,MXNK,IERR)
         IF( IERR.GT.0 ) GOTO 900
         CALL COM_STOC1
         DO
            CALL COM_STOC4(IFINISH)
            IF( IFINISH.EQ.1 ) EXIT
         ENDDO
         CALL MPI_FINALIZE(IERR)
         STOP
      ENDIF
C
      CALL INPUTD ! 漂流物データ
      CALL INPUTF ! フェンスデータ
C
C----------------------------------------
C     メモリの割り当て
C----------------------------------------
      CALL ALLOCATE_FILEIN(MXAREA,MXNIJ,MXNIJK,IERR)
      IF( IERR.GT.0 ) GOTO 900
      CALL ALLOCATE_FLUID(MXAREA,MXNIJ,MXNIJK,IERR)
      IF( IERR.GT.0 ) GOTO 900
      CALL ALLOCATE_GEOM(MXAREA,MXNI,MXNJ,IERR)
      IF( IERR.GT.0 ) GOTO 900
      CALL ALLOCATE_GRID(MXAREA,MXNI,MXNJ,MXNK,IERR)
      IF( IERR.GT.0 ) GOTO 900
C
      TIME = TSTART
      IF( IREST.EQ.1 ) THEN
         CALL INPUTR ! リスタートデータ
      ENDIF
C
C----------------------------------------
C     時間積分計算
C----------------------------------------
      CALL SOLVER
C
C
      CLOSE(IFL,STATUS='KEEP')
C<<<<< (START) STOC-DM VERSION  <<<<<<<
      CALL MPI_FINALIZE(IERR)
C<<<<<  (END)  STOC-DM VERSION  <<<<<<<
C
C
      STOP
  900 CONTINUE
      WRITE(*,*) 'ERROR END:STOC-DM'
      CALL ERRMSG('MAIN',-1)
      END
