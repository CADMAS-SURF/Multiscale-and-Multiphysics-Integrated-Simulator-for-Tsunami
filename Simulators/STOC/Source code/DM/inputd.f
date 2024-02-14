      SUBROUTINE INPUTD
C----------------------------------------
C     漂流物の条件の読込み
C----------------------------------------
      USE M_GRID
      USE M_DRIFT
      USE M_FLUID
      USE M_MODEL
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      REAL(8),PARAMETER::PI=3.141592653589793D0
      REAL(8)::AMMAX=0.0D0
      REAL(8)::XDMIN=1.0D10,XDMAX=-1.0D10
      REAL(8)::YDMIN=1.0D10,YDMAX=-1.0D10
c      REAL(8)::DXY
      INTEGER::N,NN,IERR,IFORM
      CHARACTER(256)::LINE
C
C
      OPEN (12,FILE='./drift/drift.dat',
     &                             STATUS='OLD',FORM='FORMATTED',ERR=90)
C
C ... 漂流物の数をカウント
      ND = 0
      DO N=1,99999999
         READ(12,'(A256)',END=20) LINE
         IF( LINE(1:1).NE.'#'.AND.LINE.NE.'' ) THEN
            ND=ND+1
         ENDIF
      ENDDO
   20 CONTINUE
      WRITE(IFL,*)
      WRITE(IFL,*) 'ND=',ND
      REWIND(12)
      CALL ALLOCATE_DRIFT(ND,IERR)
      IF( IERR.GT.0 ) GOTO 91
C
      ND=0
      DO N=1,99999999
C ...... ファイルの読み込み (#で始まる行は読み飛ばす)
         READ(12,'(A256)',END=10) LINE
         IF( LINE(1:1).NE.'#'.AND.LINE.NE.'' ) THEN
            ND = ND+1
            LD(ND)=0
C
            IFORM=0
            READ(LINE,*,IOSTAT=IERR) NN,TYPED(ND),
     $                 XD1(ND),YD1(ND),TD1(ND),UD1(ND),VD1(ND),OD1(ND),
     $                 AL(ND),AB(ND),AD(ND),AI(ND),
     $                 BL(ND),BB(ND),BD(ND),AZ1(ND),AZ2(ND),
     $                 HMIN_INI(ND),HMAX_INI(ND),FMAX_INI(ND),
     $                 ALPD(ND),CKD(ND),
     $                 TIME_S(ND),TIME_D(ND),DST_LIMIT(ND),
     $                 ABHOLE(ND),ATHOLE(ND),ABHOLED(ND),ATHOLED(ND),
     $                 CQD(ND),CDD(ND),TDST_INI(ND)
            IF(IERR.GT.0)THEN
            IFORM=1
            READ(LINE,*,IOSTAT=IERR) NN,TYPED(ND),
     $                 XD1(ND),YD1(ND),TD1(ND),UD1(ND),VD1(ND),OD1(ND),
     $                 AL(ND),AB(ND),AD(ND),AI(ND),
     $                 BL(ND),BB(ND),BD(ND),AZ1(ND),AZ2(ND),
     $                 HMIN_INI(ND),HMAX_INI(ND),FMAX_INI(ND),
     $                 ALPD(ND),CKD(ND),
     $                 TIME_S(ND),TIME_D(ND),DST_LIMIT(ND),
     $                 ABHOLE(ND),ATHOLE(ND),ABHOLED(ND),ATHOLED(ND),
     $                 CQD(ND),CDD(ND)
            TDST_INI(ND)=1.D+9
            ENDIF
C
            IF(IERR.GT.0)THEN
            IFORM=2
            READ(LINE,*,IOSTAT=IERR) NN,TYPED(ND),
     $                 XD1(ND),YD1(ND),TD1(ND),UD1(ND),VD1(ND),OD1(ND),
     $                 AL(ND),AB(ND),AD(ND),AI(ND),
     $                 BL(ND),BB(ND),BD(ND),AZ1(ND),AZ2(ND),
     $                 HMIN_INI(ND),HMAX_INI(ND),FMAX_INI(ND),
     $                 ALPD(ND),CKD(ND),
     $                 TIME_S(ND),TIME_D(ND),DST_LIMIT(ND)
            ABHOLE(ND)=0.D0
            ATHOLE(ND)=0.D0
            ABHOLED(ND)=0.D0
            ATHOLED(ND)=0.D0
            CQD(ND)=1.D0
            CDD(ND)=1.D0
            TDST_INI(ND)=1.D+9
            ENDIF
C
            IF(IERR.GT.0)THEN
               WRITE(*,*) 'ERROR: FORMAT ERROR FOUND IN drift.dat'
               WRITE(*,*) 'LINE=',TRIM(LINE)
               CALL ERRMSG('INPUTD',-11)
            ELSEIF( SINK_MODEL.EQ.1.AND.IFORM.EQ.2 ) THEN
               WRITE(*,*) 'ERROR: INSUFFICIENT DATA IN drift.dat'
               WRITE(*,*) '        MODIFY drift.dat OR ',
     $                    'SET MODEL=H21 IN input.dat'
               CALL ERRMSG('INPUTD',-12)
            ENDIF
C
C ......... 単位変換と喫水・慣性モーメントの計算
            TD1(ND) = TD1(ND)*PI/180.0D0
            OD1(ND) = OD1(ND)*PI/180.0D0
            AM(ND) = AD(ND)*(RHO*AL(ND)*AB(ND))
            AHIN(ND) = 0.0D0
c            WRITE(*,*) 'ND=',ND,' AM=',AM(ND)
            IF(AI(ND).LE.0.0D0) THEN
               AI(ND)=AM(ND)*(AL(ND)**2+AB(ND)**2)/12.0D0
c               WRITE(*,*) 'ND=',ND,' AI=',AI(ND)
            ENDIF
C
            IF(AZ1(ND).LT.AD(ND)) THEN
               WRITE(*,*) 'ERROR: AZ1 MUST BE GREATER THAN AD'
               WRITE(*,*) 'ND=',ND,' AD=',AD(ND),' AZ1=',AZ1(ND)
               CALL ERRMSG('INPUTD',-1)
            ENDIF
            IF(BL(ND).LT.AL(ND)) THEN
               WRITE(*,*) 'ERROR: BL MUST BE GREATER THAN AL'
               WRITE(*,*) 'ND=',ND,' BL=',BL(ND),' AL=',AL(ND)
               CALL ERRMSG('INPUTD',-2)
            ENDIF
            IF(BB(ND).LT.AB(ND)) THEN
               WRITE(*,*) 'ERROR: BB MUST BE GREATER THAN AB'
               WRITE(*,*) 'ND=',ND,' BB=',BB(ND),' AB=',AB(ND)
               CALL ERRMSG('INPUTD',-3)
            ENDIF
            IF(BD(ND).LT.AD(ND)) THEN
               WRITE(*,*) 'ERROR: BD MUST BE GREATER THAN AD'
               WRITE(*,*) 'ND=',ND,' BD=',BD(ND),' AD=',AD(ND)
               CALL ERRMSG('INPUTD',-4)
            ENDIF
            IF( SINK_MODEL.EQ.0 ) THEN
            IF(CKD(ND).LT.0.0D0) THEN
               WRITE(*,*) 'ERROR:CKD MUST BE >= 0'
               WRITE(*,*) 'ND=',ND,' CKD=',CKD(ND)
               CALL ERRMSG('INPUTD',-8)
            ENDIF
            ENDIF
            IF( SINK_MODEL.EQ.1 ) THEN
            IF(CQD(ND).LT.0.0D0) THEN
               WRITE(*,*) 'ERROR:CQD MUST BE >= 0'
               WRITE(*,*) 'ND=',ND,' CQD=',CQD(ND)
               CALL ERRMSG('INPUTD',-9)
            ENDIF
            IF(CDD(ND).LT.0.0D0) THEN
               WRITE(*,*) 'ERROR:CDD MUST BE >= 0'
               WRITE(*,*) 'ND=',ND,' CDD=',CDD(ND)
               CALL ERRMSG('INPUTD',-10)
            ENDIF
            ENDIF
C
            AM0(ND)=AM(ND)
            AI0(ND)=AI(ND)
            AD0(ND)=AD(ND)
            BD0(ND)=BD(ND)
            WSD(ND)=0.0D0
            IFLAG_S(ND)=-1
            IFLAG_D(ND)=-1
            IF( TIME_S(ND).GT.0.0D0 ) IFLAG_S(ND)=0
            IF( TIME_D(ND).GT.0.0D0 ) IFLAG_D(ND)=0
            IF( SINK_MODEL.EQ.1 ) IFLAG_D(ND)=0
C
C ......... 側面の分割数を設定
c            DXY = MIN(DX(NA),DY(NA))
c            NDDIV(1,ND) = MAX(NINT(AL(ND)/DXY*DBLE(NDIV)),1) + 1
c            NDDIV(2,ND) = MAX(NINT(AB(ND)/DXY*DBLE(NDIV)),1) + 1
            NDDIV(1,ND) = MIN(MAX(NINT(AL(ND)/LNGDIV),1) + 1, MAXDIV_L)
            NDDIV(2,ND) = MIN(MAX(NINT(AB(ND)/LNGDIV),1) + 1, MAXDIV_B)
            IF( NDDIV(1,ND).GT.MXSIDE .OR. NDDIV(2,ND).GT.MXSIDE ) THEN
               WRITE(*,*) 'ERROR: TOO FINE DIVISION BY NDIV'
               WRITE(*,*) '       ND,NDDIV(2) = ',
     $                                     ND,NDDIV(1,ND),NDDIV(2,ND)
               CALL ERRMSG('INPUTD',-5)
            ENDIF
C
            AMMAX = MAX(AMMAX,AM(ND))
            XDMIN = MIN(XDMIN,XD1(ND))
            XDMAX = MAX(XDMAX,XD1(ND))
            YDMIN = MIN(YDMIN,YD1(ND))
            YDMAX = MAX(YDMAX,YD1(ND))
         ENDIF
      ENDDO
C
   10 CONTINUE
      CLOSE(12)
C
C ... 入力データの簡易情報出力
      WRITE(IFL,*) 'INPUT DRIFT DATA END'
      WRITE(IFL,*) 'NUMBER OF SHIPS IS ',ND
      WRITE(IFL,*) 'MAX MASS IS ',AMMAX
      WRITE(IFL,*) 'X-RANGE IS ',XDMIN,' - ',XDMAX
      WRITE(IFL,*) 'Y-RANGE IS ',YDMIN,' - ',YDMAX
C
      CALL DSHIFT
      CALL VERTEX
C
      RETURN
C
   90 CONTINUE
      WRITE(*,*) 'ERROR: cannot open drift.dat'
      CALL ERRMSG('INPUTD',-6)
   91 CONTINUE
      WRITE(*,*) 'ERROR END:STOC-DM'
      CALL ERRMSG('INPUTD',-7)
      END
