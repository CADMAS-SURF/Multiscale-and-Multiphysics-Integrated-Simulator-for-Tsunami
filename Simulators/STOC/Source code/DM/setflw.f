      SUBROUTINE SETFLW(IFLAG)
C----------------------------------------
C     流動場を設定する
C----------------------------------------
      USE M_GRID
      USE M_TIME
      USE M_FILEIN,ONLY: UUAR0,VVAR0,HHAR0,UUAR1,VVAR1,HHAR1
     $   ,TIMFL0,TIMFL1,DTFL,IEOF,ONLINE,OFFLINE
      USE M_FLUID
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      INTEGER,INTENT(IN)::IFLAG
C
      REAL(8),PARAMETER::DTFLINT=1.0D+1
      REAL(8)::C1,C2
      INTEGER::I,J,K,NA
      INTEGER::NNN
C
C
      IF( OFFLINE .AND. IEOF.EQ.1 ) RETURN
C
C----------------------------------------
C     (A) 初期化処理(IFLAG==0)
C----------------------------------------
      IF( IFLAG.EQ.0 ) THEN
C
C
         IFINISH = 0
C
C<<<<< (START) STOC-DM VERSION  <<<<<<<
         CALL COM_STOC1                 !  領域情報・計算条件の通信
C<<<<<  (END)  STOC-DM VERSION  <<<<<<<
C
         IF( IREST.EQ.1 ) RETURN
      ENDIF
C
C
C----------------------------------------
C     (B) 必要なときだけ、ファイル読込み
C----------------------------------------
      IF( TIME.GT.TIMFL1 .OR. IFLAG.EQ.0 ) THEN
  800    CONTINUE
C ...... 一つ古い時刻のデータの入れ換え
         TIMFL0 = TIMFL1
         UUAR0  = UUAR1
         VVAR0  = VVAR1
         HHAR0  = HHAR1
C
C
C<<<<< (START) STOC-DM VERSION  <<<<<<<
         IF ( IFINISH.EQ.1 ) THEN
            WRITE (IFL,*) ' #########   STOC-DM   ######### '
            WRITE (IFL,*) ' Fluid calculation is finished ! '
            WRITE (IFL,*) ' #########   STOC-DM   ######### '
            RETURN
         ENDIF
         CALL COM_STOC2(TIMFL1,IEOF)              !  物理量(HT,HH,UU,VV,IDST,IBLC)の通信
C
         IF( ONLINE ) CALL COM_STOC3(TIMFL1,TEND) !  時刻の通信
         IF( ONLINE ) CALL COM_STOC4(IFINISH)     !  流体計算との同期
         IF( IFINISH.EQ.1 ) TEND=MIN(TEND,TIME)
C
         IF( IEOF.EQ.0 ) THEN
         DTFL = TIMFL1 - TIMFL0
         IF (IFLAG.EQ.0) DTFL = DTFLINT
C<<<<<  (END)  STOC-DM VERSION  <<<<<<<
C
         CALL REFER       ! 子領域の物理量を参照
C
         ELSE             ! ファイル読み込みでEOFに達したときの処理(最後に読み込んだ値で一定とする)
            UUAR1  = UUAR0
            VVAR1  = VVAR0
            HHAR1  = HHAR0
            TIMFL1 = TIME
            DTFL = TIMFL1 - TIMFL0
         ENDIF
C
         IF( TIME.GT.TIMFL1 ) GOTO 800    ! リスタート時の読み飛ばし用
      ENDIF
C
C
C----------------------------------------
C     (C) 以下、毎ステップの補間処理(IFLAG==0,1)
C----------------------------------------
      IF( IFLAG.EQ.0.OR.IFLAG.EQ.1 ) THEN
         UFAR = UFAR1
         VFAR = VFAR1
         HFAR = HFAR1
C
         C1 = (TIMFL1-TIME)/DTFL
         C2 = 1.0D0 - C1
C
C
         DO NA=1,MXAREA
C
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C  UF1,VF1,HF1  → UFAR1,VFAR1,HFAR1
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            NNN=0
            DO J=1,NJ(NA)
            DO I=1,NI(NA)
               NNN = NNN+1
               HFAR1(NA,NNN) = C1*HHAR0(NA,NNN)+C2*HHAR1(NA,NNN)
            ENDDO
            ENDDO
C
            NNN=0
            DO K=1,NK(NA)
            DO J=1,NJ(NA)
            DO I=1,NI(NA)
               NNN = NNN+1
               UFAR1(NA,NNN) = C1*UUAR0(NA,NNN)+C2*UUAR1(NA,NNN)
               VFAR1(NA,NNN) = C1*VVAR0(NA,NNN)+C2*VVAR1(NA,NNN)
            ENDDO
            ENDDO
            ENDDO
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C  UF1,VF1,HF1  → UFAR1,VFAR1,HFAR1
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
         ENDDO
C
      ENDIF
C
C
C
      RETURN
      END
