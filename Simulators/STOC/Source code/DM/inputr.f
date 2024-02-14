      SUBROUTINE INPUTR
C----------------------------------------
C     リスタートデータの読込み
C----------------------------------------
      USE M_TIME
      USE M_DRIFT
      USE M_GRID
      USE M_FILEIN
      USE M_OUTPUT,ONLY:IFL
      USE M_MODEL,ONLY:L_RAND,I_RAND_SEED
      USE M_FLUID,ONLY:UFAR1,VFAR1,HFAR1
C
      IMPLICIT NONE
C
      REAL(8),PARAMETER::EPS=1.0D-1
C
      REAL(8)::TIMEX,DTX,TIMFL1X,DTFLX,TIMWIN1X,TIMWIN0X
      INTEGER::NSX,NDX,N,NA,SIZ1,SIZ2
C
C
      OPEN (13,FILE='./drift/rin.dat',
     &                           STATUS='OLD',FORM='UNFORMATTED',ERR=90)
C
      WRITE(IFL,*) 'SEARCHING RESTART DATA'
      DO
         READ(13,END=91) TIMEX,NSX,DTX,NDX,TIMFL1X,DTFLX,
     $                   TIMWIN1X,TIMWIN0X
         WRITE(IFL,*) ' NOW, TIME=',TIMEX
         IF( ABS(TIMEX-TIME).LE.EPS ) THEN
            WRITE(IFL,*) ' READ RESTART DATA'
            WRITE(IFL,*) '    RESTART TIME =',TIMEX
            WRITE(IFL,*) '    RESTART STEP =',NSX
            IF( NDX.NE.ND ) THEN
               WRITE(*,*) 'WARNING: NUM.OF SHIPS IS NOT MATCH'
               WRITE(*,*) '         ND=',ND,NDX
            ENDIF
C
            TIME = TIMEX
            NS   = NSX
            DT   = DTX
            TIMFL1=TIMFL1X
            DTFL = DTFLX
            TIMWIN1=TIMWIN1X
            TIMWIN0=TIMWIN0X
C
            READ(13) (XD1(N),YD1(N),TD1(N),UD1(N),VD1(N),OD1(N),N=1,ND)
            READ(13) (AM(N),AI(N),AD(N),BD(N),N=1,ND)
            READ(13) (WSD(N),HZD(N),HAZ(N),AHIN(N),N=1,ND)
            READ(13) (LD(N),IFLAG_S(N),IFLAG_D(N),N=1,ND)
            DO NA=1,MXAREA
             SIZ1=NI(NA)*NJ(NA)*NK(NA)
             SIZ2=NI(NA)*NJ(NA)
             read(13) UUAR1(NA,1:SIZ1),VVAR1(NA,1:SIZ1),HHAR1(NA,1:SIZ2)
             read(13) UUAR0(NA,1:SIZ1),VVAR0(NA,1:SIZ1),HHAR0(NA,1:SIZ2)
             read(13) UFAR1(NA,1:SIZ1),VFAR1(NA,1:SIZ1),HFAR1(NA,1:SIZ2)
            ENDDO
            read(13) (xd_init(n),yd_init(n),zd_init(n),n=1,nd)
            IF( L_RAND==1 ) THEN
               read(13) I_RAND_SEED
               CALL RANDOM_SEED(PUT=I_RAND_SEED)
            ELSE
               read(13)
            ENDIF
            GOTO 10
         ELSE
            READ(13)
            READ(13)
            READ(13)
            READ(13)
c
            DO NA=1,MXAREA
              READ(13)
              READ(13)
              READ(13)
           ENDDO
c
            READ(13)
            READ(13)
         ENDIF
      ENDDO
C
   10 CONTINUE
      CLOSE(13)
C
      CALL DSHIFT
      CALL VERTEX
C
      RETURN
C
   90 CONTINUE
      WRITE(*,*) 'ERROR: cannot open rin.dat'
      CALL ERRMSG('INPUTR',-1)
   91 CONTINUE
      WRITE(*,*) 'ERROR: RESTART TIME IS NOT FOUND IN rin.dat'
      WRITE(*,*) '       TIME = ',TIME
      CALL ERRMSG('INPUTR',-2)
      END
