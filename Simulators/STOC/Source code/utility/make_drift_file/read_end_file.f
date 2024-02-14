      SUBROUTINE READ_END_FILE(N_INUND,T_INUND,T_MAXD,D_MAXD,IDST,TMDST)
!//////////////////////////////////////////////////
!     endファイルに出力されている以下のデータを読み込む
!
! # TSUNAMI TIME
! # MAX INUNDATION VALUE
! # MAX INUNDATION TIME
! # INUNDATION TIME D=  XXXX
! # DESTROY FLAG
! # DESTROY TIME
!//////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'CONTROL.h'
      INCLUDE 'DOMAIN.h'
!
      INTEGER,INTENT(IN) :: N_INUND        ! 浸水深の基準値の数
      real(8),INTENT(OUT):: T_INUND(N_INUND,MX,MY)
!                                            d_inundの到達時刻[s]
      REAL(8),INTENT(OUT):: T_MAXD(MX,MY)  ! 最大浸水深の時刻[s]
      REAL(8),INTENT(OUT):: D_MAXD(MX,MY)  ! 最大浸水深[m]
!
!     (STOC-DS-MODE時)
      INTEGER,INTENT(OUT):: IDST(MX,MY)    ! 破壊フラグ
      REAL(8),INTENT(OUT):: TMDST(MX,MY)   ! 破壊時刻
!
      INTEGER:: I1,I2,I3,I4,NI ! check flag
      INTEGER:: I,J
      REAL(8):: DUM
!
!
      I1=0
      I2=0
      I3=0
      I4=0
      NI=1
!
      IDST(:,:)=0
      TMDST(:,:)=0.D0
!
      OPEN(INP,FILE=TRIM(ENDFILE),STATUS='OLD',FORM='FORMATTED',
     $     ERR=99)
!
      DO
         READ(INP,'(A132)',END=100) CLINE
         WRITE(LP,*) '  LINE=',TRIM(CLINE)
!
         IF( CLINE.EQ.' # TSUNAMI TIME' ) THEN
            I1=1
            READ(INP,610) ((T_INUND(1,I,J),I=1,MX),J=1,MY)
         ELSEIF( CLINE.EQ.' # MAX INUNDATION VALUE' ) THEN
            I2=1
            READ(INP,610) ((D_MAXD(I,J),I=1,MX),J=1,MY)
         ELSEIF( CLINE.EQ.' # MAX INUNDATION TIME' ) THEN
            I3=1
            READ(INP,610) ((T_MAXD(I,J),I=1,MX),J=1,MY)
         ELSEIF( CLINE(1:21).EQ.' # INUNDATION TIME D=' ) THEN
            NI=NI+1
            READ(INP,610) ((T_INUND(NI,I,J),I=1,MX),J=1,MY)
         ELSEIF( CLINE.EQ.' # DESTROY FLAG' ) THEN
            I4=1
            READ(INP,620) ((IDST(I,J),I=1,MX),J=1,MY)
         ELSEIF( CLINE.EQ.' # DESTROY TIME' ) THEN
            READ(INP,610) ((TMDST(I,J),I=1,MX),J=1,MY)
         ELSE
!           読み飛ばし
            READ(INP,610) ((DUM,I=1,MX),J=1,MY)
         ENDIF
      ENDDO
  610 FORMAT(10E10.3)
  620 FORMAT(10I10)
!
  100 CONTINUE
      WRITE(LP,*) 'DONE (READING END FILE)'
      WRITE(LP,*) ''
      CLOSE(INP)
!
      IF( I4.EQ.0 ) WRITE(LP,*) 'STOC-DS-MODE IS OFF'
      IF( I4.EQ.1 ) WRITE(LP,*) 'STOC-DS-MODE IS ON'
!
      IF( I1.EQ.0 ) THEN
         CALL ERRMSG('READ_END_FILE',71)
         WRITE(*,*) '# TSUNAMI TIME IS NOT FOUND IN ',TRIM(ENDFILE)
         CALL ABORT1('')         
      ENDIF
      IF( I2.EQ.0 ) THEN
         CALL ERRMSG('READ_END_FILE',72)
         WRITE(*,*) '# MAX INUNDATION VALUE IS NOT FOUND IN ',
     $      TRIM(ENDFILE)
         CALL ABORT1('')         
      ENDIF
      IF( I3.EQ.0 ) THEN
         CALL ERRMSG('READ_END_FILE',73)
         WRITE(*,*) '# MAX INUNDATION TIME IS NOT FOUND IN ',
     $      TRIM(ENDFILE)
         CALL ABORT1('')         
      ENDIF
!
      IF( NI.NE.N_INUND ) THEN
         CALL ERRMSG('READ_END_FILE',74)
         WRITE(*,*) 'NI AND N_INUND DO NOT MATCH'
         WRITE(*,*) 'NI     =',NI
         WRITE(*,*) 'N_INUND=',N_INUND
         CALL ABORT1('')         
      ENDIF
!
      RETURN
!
   99 CONTINUE
      CALL ERRMSG('READ_END_FILE',41)
      WRITE(*,*) 'CANNOT OPEN ',TRIM(ENDFILE)
      CALL ABORT1('')
      END
