      SUBROUTINE SEABOT(HU,HV,UU,VV,FF,HH,HDEP,GX,GY,XC,YC,ZC,YCOSP,
     $                  INDU,INDV,INDP,KF,KP,KG)
C======================================================================
C     海底変動量時間変化処理
C===============================================
      USE MOD_FAULT,ONLY: NFLT,NFLTNOW,FPARAM
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'FILEC.h'
C
      REAL(8),INTENT(IN)::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(IN)::GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOSP(MY)
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ),INDV(MX,MY,MZ),INDP(MX,MY,MZ)
      INTEGER,INTENT(IN)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),HH(MX,MY),HDEP(MX,MY)
C
      INTEGER,PARAMETER::IDB=1,NFL=1
      INTEGER::I,J,IS,IE,JS,JE,K,IS1,IE1,JS1,JE1
      INTEGER::INSIDE,IRTN
      REAL(8)::TIMESB,FFI,FFJ,FF0,FF1,FF2,GX1,GY1,HUD,HVD
      REAL(8)::TIM1
      SAVE TIMESB,IS,IE,JS,JE
C     局所配列
      REAL(8)::DELH(MX,MY),dummy
      REAL(8),PARAMETER:: WATERLEVEL=0.0d0 ! 陸地での地盤変動をなくすときはここの値を潮位に設定
      INTEGER,SAVE::INITX=0,ISBTFORM=0,IFLSBX
      CHARACTER(64),SAVE::SUBFILE
C
C
      IF(LSURF.EQ.0.OR.NBOT.EQ.0) RETURN
      TIM1=TIME-0.5D0*DTV
C
C ... 時間積分ループに入る前に1回だけ行う処理
      IF( INITX.EQ.0 ) THEN
        IFLSBX=IFLSB
C
        IF( NBOT.EQ.-1 ) THEN
           CALL FAULTI(TIME,TIMESB,TIM1)
C
        ELSE
! 1行目を読み込んでフォーマットをチェック(従来:ISBTFORM=0,分割形式:ISBTFORM=1)
           READ(IFLSB,'(A132)',END=900) CLINE
           REWIND IFLSB
           READ(CLINE,*,ERR=10,END=10) TIMESB,IS,IE,JS,JE,SUBFILE
           ISBTFORM=1
           IFLSBX=IFLSB2
   10      CONTINUE
C
C
C       リスタート時はリスタート時刻以前のデータを読み飛ばすためにループ処理を行う
  100      CONTINUE
C
           IF(ISBTFORM.EQ.0) THEN
              READ(IFLSB,*,END=900) TIMESB,IS,IE,JS,JE
           ELSE
              READ(IFLSB,*,END=900) TIMESB,IS,IE,JS,JE,SUBFILE
           ENDIF
C
           IF(IDB.NE.0) THEN
              WRITE(LP,*) 'SEABOT: SKIP AT TIME,TIMESB =',TIME,TIMESB
C           WRITE(6,*) '0   TIME,TIMESB =',TIME,TIMESB
C           WRITE(6,*) '    IS,IE       =',IS,IE
C           WRITE(6,*) '    JS,JE       =',JS,JE
           END IF
C
C ..... 0.0秒で水位変動が設定されたときはすぐに変動量を反映させる
           IF(TIME.EQ.TIMESB.AND.TIMESB.EQ.0.0D0) THEN
              TIM1=TIMESB
           ELSEIF(TIME.GE.TIMESB) THEN
              IF(ISBTFORM.EQ.0) READ(IFLSB,*) ((dummy,i=IS,IE),J=JS,JE)
              GO TO 100
           END IF
        ENDIF
C
        INITX=1
      END IF
C
      IF(TIM1.LT.TIMESB) RETURN
C
C ... 変動量の読込
      IS1=IS
      IE1=IE
      JS1=JS
      JE1=JE
      CALL ZERCLR(DELH,MXY,0.0D0)
C
      IF( NBOT.EQ.-1 ) THEN
         CALL FAULTT(DELH,TIM1)
         IS1=2
         IE1=MXM
         JS1=2
         JE1=MYM
      ELSE
C ... 分割形式の場合、一時ファイルを開く
      IF( ISBTFORM.EQ.1 )
     $   OPEN(IFLSBX,FILE=trim(SUBFILE),FORM='FORMATTED',STATUS='OLD')
C
      IF(IAUTOD.EQ.0) THEN
         READ(IFLSBX,*,END=910) ((DELH(I,J),i=IS,IE),J=JS,JE)
      ELSE
         CALL RDSUB2RAX(DELH,IFLSBX,0,IS1,IE1,JS1,JE1,INSIDE,IRTN)
         IF(IRTN.EQ.2) GOTO 910
         IF(INSIDE.EQ.0) GOTO 700
      ENDIF
C
C ... 分割形式の場合、一時ファイルを閉じる
      IF( ISBTFORM.EQ.1 ) CLOSE(IFLSBX)
      ENDIF
C
      WRITE(LP,*) 'SEABOT: DELH ADD AT TIM1,TIMESB =',TIM1,TIMESB
      DO 200 J=JS1,JE1
      DO 200 I=IS1,IE1
        IF(KF(I,J).LT.MZ) THEN
          IF( HDEP(I,J).LT.WATERLEVEL ) THEN
          HH(I,J) = MAX(HH(I,J)+DELH(I,J),HDEP(I,J)+EPSH)
          DO 250 K=2,MZM
            IF(HH(I,J).GE.ZC(1,K)) THEN
              FF(I,J,K) = 1.0D0
            ELSE IF(HH(I,J).LT.ZC(1,K-1)) THEN
              FF(I,J,K) = 0.0D0
            ELSE
              FF(I,J,K) = (HH(I,J)-ZC(1,K-1))*ZC(6,K)
            END IF
  250     CONTINUE
          endif
        ELSE
          IF(DELH(I,J).NE.0.0D0) THEN
ccc            WRITE(6,600) TIMESB,I,J,DELH(I,J)
  600       FORMAT('### SEA-BOOTOM FILE TIME =',1P,D12.5,
     $             '  LAND(I,J),DELH=',2I5,1P,D12.5)
            DELH(I,J) = 0.0D0
          END IF
        END IF
  200 CONTINUE
C
      CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
C
C ... 単層領域のみ 
      IF(MZ.EQ.3) THEN
        IF(NFL.EQ.0) THEN
          DO 300 J=JS1-1,JE1
          DO 300 I=IS1-1,IE1
C
            IF(J.GT.JS1-1) THEN
              IF(DELH(I,J).NE.0.0.OR.DELH(I+1,J).NE.0.0D0) THEN
                K = MAX(KF(I,J),KF(I+1,J))
cc                write(6,*) 'uu i,j=',i,j
cc                write(6,*) K,KF(I,J),KF(I+1,J)
                IF(INDU(I,J,K).EQ.1) THEN                        
                  GX1 = 1.0D0-GX(I,J,K)
                  FFI = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
                  FF0 = MAX(FFI        -GX1,0.0D0)
                  FF1 = MAX(FF(I  ,J,K)-GX1,0.0D0)
                  FF2 = MAX(FF(I+1,J,K)-GX1,0.0D0)
                  HUD = PARAMF2*FF0*UU(I,J,K)
     $                + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $                +         FF2*MIN(UU(I,J,K),0.0D0))
                  IF(HUD.NE.0.0) THEN
                    UU(I,J,K) = UU(I,J,K)*HU(I,J,K)/HUD
                  ELSE
                    UU(I,J,K) = 0.0D0
                  END IF
                END IF
              END IF
            END IF
C
            IF(I.GT.IS1-1) THEN 
              IF(DELH(I,J).NE.0.0D0.OR.DELH(I,J+1).NE.0.0D0) THEN
                K = MAX(KF(I,J),KF(I,J+1))
                IF(INDV(I,J,K).EQ.1) THEN
                  GY1 = 1.0D0-GY(I,J,K)
                  FFJ = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
                  FF0 = MAX(FFJ        -GY1,0.0D0)
                  FF1 = MAX(FF(I,J  ,K)-GY1,0.0D0)
                  FF2 = MAX(FF(I,J+1,K)-GY1,0.0D0)
                  HVD = PARAMF2*FF0*VV(I,J,K)
     $                + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $                +         FF2*MIN(VV(I,J,K),0.0D0))
                  HVD = HVD*YCOSP(J)
                  IF(HVD.NE.0.0D0) THEN
                    VV(I,J,K) = VV(I,J,K)*HV(I,J,K)/HVD
                  ELSE
                    VV(I,J,K) = 0.0D0
                  END IF
                END IF
              END IF
            END IF
  300     CONTINUE
        END IF       
C
      ELSE IF( NFL.EQ.0 ) THEN
        CALL ERRMSG('SEABOT',7190)
        WRITE(LP,*) '#### CAN NOT USE THIS MODEL(MZ>3) ###   STOP'
        CALL ABORT1('')
        RETURN
      END IF
C
  700 CONTINUE
C
      IF( NBOT.EQ.-1 ) THEN
         IF(NFLTNOW.GT.NFLT) THEN
            GOTO 900
         ELSE
            TIMESB=FPARAM(10,NFLTNOW)
         ENDIF
      ELSEIF( NBOT.EQ.1 ) THEN
      IF(ISBTFORM.EQ.0) THEN
         READ(IFLSB,*,END=900) TIMESB,IS,IE,JS,JE
      ELSE
         READ(IFLSB,*,END=900) TIMESB,IS,IE,JS,JE,SUBFILE
      ENDIF
      ENDIF
C
      IF(IDB.NE.0) THEN
         WRITE(LP,*) 'SEABOT: NEXT TIMESB =',TIMESB
      END IF
C
      RETURN
C
  900 CONTINUE
      WRITE(6,*) '### SEABOT: READ END OF SEA-BOTOOM FILE ###'
      WRITE(LP,*) '### SEABOT: READ END OF SEA-BOTOOM FILE ###'
      NBOT = 0
      RETURN
C
C ... エラー処理
  910 CONTINUE
      CALL ERRMSG('SEABOT',7191)
      WRITE(LP,*) '### SEA-BOTOOM FILE FORMAT ERROR ###   STOP'
      WRITE(LP,*) '    TIME,TIMESB =',TIME,TIMESB
      WRITE(LP,*) '    IS,IE       =',IS,IE
      WRITE(LP,*) '    JS,JE       =',JS,JE
      CALL ABORT1('')
      RETURN
      END
