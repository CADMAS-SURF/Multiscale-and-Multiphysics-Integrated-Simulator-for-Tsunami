      SUBROUTINE MKINIT(UU,VV,WW,PP,TT,CC,X1,X2,TMU,
     1                  INDP,INDU,INDV,INDW,HDEP,AMNG,
     $                  FF,HH,PATM,WX,WY,KF,KG,KP,
     $                  XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,GV,GX,GY,
     $                  VLEND,TMEND,FREND,HHBCN,UUBCN,VVBCN,
     $                  HTDST1,HTDST2,IDST)
C======================================================================
C     初期値を設定する
C       LTUR=2(k-εモデル):X1=AK,X2=EP
C       LTUR=3(M-Y モデル):X1=Q2,X2=QL
C       LTUR=4(SGS モデル):X1=AK,X2=--
C======================================================================
      IMPLICIT NONE
      REAL(8),PARAMETER::GVMIN=0.0D-4
C
      INCLUDE 'OBSTI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'INITL.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'GRID.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'OUTPUT.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::X1(MX,MY,MZ),X2(MX,MY,MZ),TMU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY)
      REAL(8),INTENT(IN)::XC(8,MX,MY),XCP(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY),YSIN(MY),YSINP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::VLEND(MX,MY,16),TMEND(MX,MY,6)
      REAL(8),INTENT(INOUT)::FREND(MX,MY,2+NFRAGL)
      REAL(8),INTENT(INOUT)::HHBCN(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
C
      CHARACTER(10)::CTMP
C
      REAL(8),INTENT(INOUT)::HTDST1(MX,MY),HTDST2(MX,MY)
      INTEGER,INTENT(INOUT)::IDST(MX,MY)
C
      REAL(8)::DR1,DT1,DTOLD1,HDDD,HDPMAX,HH1,TIME1,VLDEP,GV1
      REAL(8)::RTMP,ZZ1
      INTEGER,PARAMETER:: LMODHDEP=0
      INTEGER::I,IE,IERR,II,IS,ISTEP1,J,JE,JJ,JS,K,KG1,KT1,IRTN
      INTEGER::LCONC1,LSURF1,LTEMP1,LTURB1,LDUM1,N,KOUNT
C     局所配列変数
      REAL(8),ALLOCATABLE::GVH(:,:,:)
      INTEGER,ALLOCATABLE::KDEP(:,:)
C
      INTEGER::IPARNT
      IPARNT=IPECON(2,NRANK+1)
C
      CALL ZERCLR(UU,MXYZ,0.0D0)
      CALL ZERCLR(VV,MXYZ,0.0D0)
      CALL ZERCLR(WW,MXYZ,0.0D0)
      CALL ZERCLR(PP,MXYZ,0.0D0)
      CALL ZERCLR(TT,MXYZ,0.0D0)
      CALL ZERCLR(CC,MXYZ,0.0D0)
      CALL ZERCLR(X1,MXYZ,0.0D0)
      CALL ZERCLR(X2,MXYZ,0.0D0)
      CALL ZERCLR(TMU,MXYZ,0.0D0)
      CALL ZERCLR(FF,MXYZ,0.0D0)
      CALL ZERCLR(HH,MXY,0.0D0)
      CALL ZERCLR(HDEP,MXY,0.0D0)
      CALL ZERCLR(PATM,MXY,0.0D0)
      CALL ZERCLR(WX,MXY,0.0D0)
      CALL ZERCLR(WY,MXY,0.0D0)
      CALL ZERCLR(VLEND,MXY*16,0.0D0)
      CALL ZERCLR(TMEND,MXY*6,0.0D0)
      CALL ZERCLR(FREND,MXY*(2+NFRAGL),0.D0)
      CALL ZERCLI(KF,MXY,0)
      CALL ZERCLI(KG,MXY,0)
      CALL ZERCLI(KP,MXY,0)
C
      ALLOCATE(GVH(MX,MY,MZ),KDEP(MX,MY),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('MKINIT',7030)
        WRITE(LP,*) 'CANNOT ALLOCATE GVH,KDEP'
        CALL ABORT1('')
      END IF
C
      DO 450 K=1,MZ
      DO 450 J=1,MY
      DO 450 I=1,MX
        GVH(I,J,K) = GV(I,J,K)
  450 CONTINUE
      CALL ZERCLI(KDEP,MXY,0)
C
C
C ... 海底を設定
C
      CALL ZERCLI(KG,MXY,MZ)
C
      DO 500 J=2,MYM
      DO 500 I=2,MXM
         DO 510 K=2,MZM
            IF( INDP(I,J,K).GT.0 ) THEN
               KG(I,J) = K
               VLDEP = ZC(1,K)-GV(I,J,K)*ZC(4,K)
               IF(DABS(VLDEP).LT.1.0D-5) VLDEP=0.0D0
               VLEND(I,J,1) = VLDEP
               GO TO 520
            END IF
  510    CONTINUE
         VLEND(I,J,1) = ZC(1,MZM)
  520    CONTINUE
         HDEP(I,J) = VLEND(I,J,1)
  500 CONTINUE
CC      WRITE(LP,*) 'CAL. DEPTH ='
CC      IF(LOBST.EQ.0) CALL DBWR2D(VLEND,3,1,MX,MY,16,LP)
C
C
C----------------------------------------------------------------------
C     (1) 入力データから初期条件の設定を行う
C----------------------------------------------------------------------
C
C...MOKEI ( 2004.03 ) ...............................
C     流速場のみ初期値ファイルを読む
C      IF( LSTART.EQ.0 ) THEN
      IF( ISTEP.LT.0 ) THEN
        LSTART=ISTEP
        ISTEP = 0
      END IF
      WRITE( LP,* ) 'ISTEP,LSTART(OUT)=',ISTEP,LSTART
      IF( LSTART.LE.0 ) THEN
C............................................... END
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            PP(I,J,K) = 0.0D0
            TT(I,J,K) = TTINIT
            CC(I,J,K) = CCINIT
            IF( LTURB.EQ.2 ) THEN
              X1(I,J,K) = MAX(AKINIT,AKMIN)
              X2(I,J,K) = MAX(EPINIT,EPMIN)
              IF(X2(I,J,K).NE.0.0D0) THEN
                TMU(I,J,K) = CMU*X1(I,J,K)**2/X2(I,J,K)
              END IF
            ELSE IF(LTURB.EQ.3) THEN
              X1(I,J,K) = MAX(AKINIT,Q2MIN)
              X2(I,J,K) = X1(I,J,K)*MAX(EPINIT,RLMIN)
            ELSE IF(LTURB.EQ.4) THEN
              X1(I,J,K) = MAX(AKINIT,AKMIN)
            END IF
         END IF
  100 CONTINUE
C
C ... 温度と濃度の初期値をファイルから入力する
      IF(TTINIT.GT.1.0D10.OR.CCINIT.GT.1.0D10) THEN
        REWIND(IFINI)
        DO N=1,100000
           READ(IFINI,'(A10)',END=105) CTMP
           IF( CTMP(1:2).EQ.'TT' ) THEN
              WRITE(6,*) 'read TT'
              IF(IAUTOD.EQ.0)THEN
                 READ(IFINI,*) (((TT(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
              ELSE
                 CALL RDSUB3RA(TT,IFINI,0,IRTN)
              ENDIF
           ELSE IF( CTMP(1:2).EQ.'CC' ) THEN
              WRITE(6,*) 'read CC'
              IF(IAUTOD.EQ.0)THEN
                 READ(IFINI,*) (((CC(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
              ELSE
                 CALL RDSUB3RA(CC,IFINI,0,IRTN)
              ENDIF
           ELSE
              CALL ERRMSG('MKINIT',7031)
              WRITE(LP,*) 'READ ERROR: IFINI : MKINIT'
              CALL ABORT1('')
           ENDIF
        ENDDO
  105   CONTINUE
C
        DO 110 K=2,MZM
        DO 110 J=2,MYM
        DO 110 I=2,MXM
          IF(INDP(I,J,K).EQ.0) TT(I,J,K)=0.0D0
          IF(INDP(I,J,K).EQ.0) CC(I,J,K)=0.0D0
  110   CONTINUE
      END IF
C
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=1,MXM
C ...... 計算点及び自由流入出境界
         IF( INDU(I,J,K).GE.0 ) THEN
            UU(I,J,K) = UUINIT
         END IF
  200 CONTINUE
C
      DO 300 K=2,MZM
      DO 300 J=1,MYM
      DO 300 I=2,MXM
C ...... 計算点及び自由流入出境界
         IF( INDV(I,J,K).GE.0 ) THEN
            VV(I,J,K) = VVINIT
         END IF
  300 CONTINUE
C
      DO 400 K=1,MZM
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C ...... 計算点及び自由流入出境界
         IF( INDW(I,J,K).GE.0 ) THEN
            WW(I,J,K) = WWINIT
         END IF
  400 CONTINUE
C
C
C ... 水面を設定
      CALL ZERCLR(FF,MXYZ,1.0D0)
      CALL ZERCLR(HH,MXY,ZC(1,MZM))
      CALL ZERCLI(KF,MXY,MZ)
      CALL ZERCLI(KP,MXY,MZ)
C
      IF( LSURF.EQ.1 ) THEN
         DO 600 N=1,NHINIT
            IS = IHINIT(1,N)
            IE = IHINIT(2,N)
            JS = IHINIT(3,N)
            JE = IHINIT(4,N)
            IF( IS.EQ.0 ) THEN
               IS = 2
               IE = MXM
               JS = 2
               JE = MYM
            END IF
            IF( HHINIT(N).LT.ZC(1,1) .OR. HHINIT(N).GT.ZC(1,MZM) ) THEN
               CALL ERRMSG('MKINIT',7032)
               WRITE(LP,*) 'INITIAL WATER LEVEL IS OVER ZRANGE'
               CALL ABORT1('')
            END IF
C
            IF(LOBST.EQ.0.AND.LTYPH.NE.0) CALL SRGSET(PATM,HH,XC,YC,ZC)
C
            DO 610 J=JS,JE
            DO 610 I=IS,IE
               KF(I,J) = MZ
               DO 620 K=KG(I,J),MZM
C
C ............... 水面設定前
                  IF( KF(I,J).EQ.MZ ) THEN
                    IF( INDP(I,J,K).GT.0 ) THEN
                      IF(LTYPH.NE.0) THEN
                        HH1 = HH(I,J)+HHINIT(N)
                      ELSE
                        HH1 = HHINIT(N)
                      END IF
                      IF(HH1.LT.ZC(1,K)) THEN
                        IF(INDP(I,J,K-1).EQ.0) THEN
                          HH1 = MAX(HH1,ZC(1,K)-GV(I,J,K)*ZC(4,K))
                        END IF
                        FF(I,J,K) = ( HH1 - ZC(1,K-1) )*ZC(6,K)
                        HH(I,J) = HH1
                        KF(I,J) = K
                        KP(I,J) = K
                      ELSE
                        FF(I,J,K) = 1.0D0
                      END IF
                    END IF
C
C ............... 水面設定後
                  ELSE
                     FF(I,J,K) = 0.0D0
                  END IF
  620          CONTINUE
  610       CONTINUE
C
  600    CONTINUE
      END IF
C324
      IF( LOBST.EQ.1 ) THEN
        IF(IAUTOD.EQ.0)THEN
           READ(IFLST,END=320,ERR=920) ((HDEP(I,J),I=2,MXM),J=2,MYM)
           READ(IFLST,ERR=930) ((HH  (I,J),I=2,MXM),J=2,MYM)
           READ(IFLST,ERR=940) ((AMNG(I,J),I=2,MXM),J=2,MYM)
           IF( LSTOCDS.EQ.1 ) THEN
              READ(IFLST,ERR=920) ((IDST(I,J),I=2,MXM),J=2,MYM)
              READ(IFLST,ERR=920) ((HTDST1(I,J),I=2,MXM),J=2,MYM)
              READ(IFLST,ERR=920) ((HTDST2(I,J),I=2,MXM),J=2,MYM)
           ENDIF
C
        ELSE
           CALL RDSUB2RB(HDEP,IFLST,0,IRTN)
            IF(IRTN.EQ.1) GOTO 920
            IF(IRTN.EQ.2) GOTO 320
           CALL RDSUB2RB(HH,IFLST,0,IRTN)
            IF(IRTN.EQ.1) GOTO 930
           CALL RDSUB2RB(AMNG,IFLST,0,IRTN)
            IF(IRTN.EQ.1) GOTO 940
            IF( LSTOCDS.EQ.1 ) THEN
              CALL RDSUB2IB(IDST,IFLST,0,IRTN)
              IF(IRTN.EQ.1) GOTO 920
              CALL RDSUB2RB(HTDST1,IFLST,0,IRTN)
              IF(IRTN.EQ.1) GOTO 920
              CALL RDSUB2RB(HTDST2,IFLST,0,IRTN)
              IF(IRTN.EQ.1) GOTO 920
           ENDIF
        ENDIF
        CLOSE(IFLST)
C
C ..... 水深を制限する
C      (GVの変更)
        KOUNT = 0
        DO 70 J=2,MYM
        DO 70 I=2,MXM
          IF(KF(I,J).EQ.MZ) GOTO 70
          IF(HDEP(I,J).LE.0.0D0.AND.HDEP(I,J).GT.HLMT) THEN
            KDEP(I,J) = 1
            HDEP(I,J) = HLMT
            KOUNT = KOUNT+1
            DO 72 K=2,MZM
              IF(HDEP(I,J).GT.ZC(1,K)) THEN
                INDP(I,J,K) = 0
                GVH(I,J,K) = 1.0D0
              ELSE IF(HDEP(I,J).LT.ZC(1,K-1)) THEN
                INDP(I,J,K) = 1
                GVH(I,J,K) = 1.0D0
              ELSE
                GVH(I,J,K) = (ZC(1,K)-HDEP(I,J))*ZC(6,K)
                IF(GVH(I,J,K).LT.GVMIN) THEN
                  INDP(I,J,K) = 0
                  GVH(I,J,K) = 1.0D0
                ELSE
                  INDP(I,J,K) = 1
                  GVH(I,J,K) = MIN(GVH(I,J,K),1.0D0)
                END IF
              END IF
   72       CONTINUE
          END IF
   70   CONTINUE
C
C      (GX,GY,GVの変更）
        IF(KOUNT.GT.0) THEN
          DO 74 J=2,MYM
          DO 74 I=2,MXM
            IF(KF(I,J).EQ.MZ.OR.KF(I+1,J).EQ.MZ) GO TO 77
            IF(KDEP(I,J).EQ.0.AND.KDEP(I+1,J).EQ.0) GO TO 77
            DO 76 K=2,MZM
              GV1 = XC(7,I,J)*GV(I,J,K)+XC(8,I,J)*GV(I+1,J,K)
              IF(GX(I,J,K).GE.GV1) THEN
                GX(I,J,K) = XC(7,I,J)*GVH(I,J,K)+XC(8,I,J)*GVH(I+1,J,K)
              ELSE
              END IF
   76       CONTINUE
C
   77       CONTINUE
            IF(KF(I,J).EQ.MZ.OR.KF(I,J+1).EQ.MZ) GO TO 74
            IF(KDEP(I,J).EQ.0.AND.KDEP(I,J+1).EQ.0) GO TO 74
            DO 78 K=2,MZM
              GV1 = YC(7,J)*GV(I,J,K)+YC(8,J)*GV(I,J+1,K)
              IF(GY(I,J,K).GE.GV1) THEN
                GY(I,J,K) = YC(7,J)*GVH(I,J,K)+YC(8,J)*GVH(I,J+1,K)
              ELSE
              END IF
   78       CONTINUE
   74     CONTINUE
          DO 79 K=2,MZM
          DO 79 J=2,MYM
          DO 79 I=2,MXM
            GV(I,J,K)=GVH(I,J,K)
   79     CONTINUE
        END IF
        IF( LMODHDEP.EQ.1 ) THEN
        DO J=2,MYM
        DO I=2,MXM
           IF(KF(I,J).LT.MZ) THEN
C
              ZZ1=ZC(1,1)
              DO K=2,MZM
                 IF( INDP(I,J,K).EQ.0 ) THEN
                    ZZ1=ZZ1+ZC(4,K)
                 ELSE
                    ZZ1=ZZ1+(1.0D0-GV(I,J,K))*ZC(4,K)
                 ENDIF
              ENDDO
              IF( ABS(ZZ1-HDEP(I,J)).GT.EPSH ) THEN
                 HDEP(I,J)=ZZ1
                 IF( HDEP(I,J).GT.0.0D0 ) HH(I,J)=MIN(HH(I,J),HDEP(I,J))
              ENDIF
           ENDIF
        ENDDO
        ENDDO
        ENDIF
C@@@@@@@@@@@@@@@@@@
         DO 90 J=2,MYM
           IF (NSOMER(2).GT.0) HDEP(2    ,J)=HDEP(3    ,J)
           IF (NSOMER(3).GT.0) HDEP(MXM  ,J)=HDEP(MXM-1,J)
           IF (NSOMER(2).GT.0) HH  (2    ,J)=HH  (3    ,J)
           IF (NSOMER(3).GT.0) HH  (MXM  ,J)=HH  (MXM-1,J)
           IF (NSOMER(2).GT.0) AMNG(2    ,J)=AMNG(3    ,J)
           IF (NSOMER(3).GT.0) AMNG(MXM  ,J)=AMNG(MXM-1,J)
 90      CONTINUE
         DO 91 I=2,MXM
           IF (NSOMER(1).GT.0) HDEP(I,2    )=HDEP(I,3    )
           IF (NSOMER(4).GT.0) HDEP(I,MYM  )=HDEP(I,MYM-1)
           IF (NSOMER(1).GT.0) HH  (I,2    )=HH  (I,3    )
           IF (NSOMER(4).GT.0) HH  (I,MYM  )=HH  (I,MYM-1)
           IF (NSOMER(1).GT.0) AMNG(I,2    )=AMNG(I,3    )
           IF (NSOMER(4).GT.0) AMNG(I,MYM  )=AMNG(I,MYM-1)
 91      CONTINUE
C
C ............... 水面条件の再設定
         CALL ZERCLR(FF,MXYZ,1.0D0)
         CALL ZERCLI(KF,MXY,MZ)
         CALL ZERCLI(KP,MXY,MZ)
C ....................  台風の初期水位をセットする
         IF(LTYPH.NE.0) CALL SRGSET(PATM,HH,XC,YC,ZC)
C
         DO 92 J=2,MYM
         DO 92 I=2,MXM
            IF(HDEP(I,J).LT.0.0D0) THEN
              HH1 = HH(I,J)+HHINIT(1)
            ELSE
              HH1 = HH(I,J)
              IF(HDEP(I,J).LT.HHINIT(1)) HH1=HHINIT(1)
            END IF
            IF(HH1.LT.HDEP(I,J)+EPSH) HH1=HDEP(I,J)+EPSH
            IF(HH(I,J).GT.1.0D10    ) HH1=HDEP(I,J)+EPSH
            KG1 = KG(I,J)
            IF(HH1.LT.ZC(1,KG1-1)) THEN
              HDEP(I,J) = ZC(1,KG1)-GV(I,J,KG1)*ZC(4,KG1)
              HH1 = HDEP(I,J)+EPSH
            END IF
            HH(I,J) = HH1
            DO 93 K=KG(I,J),MZM
C
              IF(KF(I,J).EQ.MZ) THEN
                IF(INDP(I,J,K).GT.0) THEN
                  IF( ZC(1,K) .LE. HH(I,J) ) THEN
                    FF(I,J,K) = 1.0D0
                  ELSE IF( ZC(1,K-1) .GT. HH(I,J) ) THEN
                    FF(I,J,K) = 0.0D0
                  ELSE
                    FF(I,J,K) = ( HH1 - ZC(1,K-1) )*ZC(6,K)
                    KF(I,J) = K
                    KP(I,J) = K
                  END IF
                END IF
              ELSE
                FF(I,J,K) = 0.0D0
              END IF
  93       CONTINUE
           IF(KF(I,J).EQ.MZ) HH(I,J)=ZC(1,MZM)
           IF(KF(I,J).EQ.MZ) HDEP(I,J)=ZC(1,MZM)
  92     CONTINUE
C
         HDPMAX = 1.0D10
         DO 94 J = 2,MYM
         DO 94 I = 2,MXM
           IF(KF(I,J).NE.MZ) THEN
             HDPMAX = MIN(HDEP(I,J),HDPMAX)
           END IF
 94      CONTINUE
         WRITE(6,*) '### HDEP MAX =',HDPMAX
         WRITE(LP,*) '### HDEP MAX =',HDPMAX
C
CC         CALL DBWR2D(HDEP,3,1,MX,MY,1,LP)
C@@@@@@@@@@@@@@@@@@
 320     CONTINUE
      ENDIF
C324
C
C ... 静水圧近似(ポロシティがあれば追加必要)
      IF( LSURF.EQ.1 ) THEN
        DO 700 J=2,MYM
        DO 700 I=2,MXM
          IF( KF(I,J).NE.MZ ) THEN
            DO 710 K=KF(I,J),2,-1
              IF( K.EQ.KF(I,J) ) THEN
                PP(I,J,K) = PATM(I,J)
     1                    + ( 0.5D0 - FF(I,J,K) )*RHO*GRAV*ZC(4,K)
              ELSE
                IF(INDP(I,J,K).NE.0) THEN 
                  IF(INDP(I,J,K+1).NE.0) THEN
                    PP(I,J,K) = PP(I,J,K+1) - RHO*GRAV*ZC(3,K)
                  ELSE
                    DO 705 JJ=2,J
                    DO 705 II=2,I
                      IF(JJ.NE.J.OR.II.NE.I.AND.INDP(II,JJ,K).NE.0) THEN
                        PP(I,J,K) = PP(II,JJ,K)
                      END IF
  705               CONTINUE
                  END IF
                ELSE
                  PP(I,J,K) = 0.0D0
                END IF
              END IF
  710       CONTINUE
          ELSE
            DO 720 K=MZM,2,-1
              IF( INDP(I,J,K).GT.0 ) KF(I,J)=-MZ
  720       CONTINUE
          END IF
          VLEND(I,J,2) = HH(I,J)
  700   CONTINUE
C
        DO 730 J=2,MYM
        DO 730 I=2,MXM
          IF( KF(I,J).EQ.-MZ ) THEN
            DO 740 K=MZM,2,-1
              IF( INDP(I,J,K).GT.0 ) THEN
                IF( KF(I,J).EQ.-MZ ) THEN
                  DO 750 JJ=2,MYM
                  DO 750 II=2,MXM
                    IF( KF(II,JJ).NE.MZ.AND.INDP(II,JJ,K).GT.0 ) THEN
                      PP(I,J,K) = PP(II,JJ,K)
                      KF(I,J) = MZ
                    END IF
  750             CONTINUE
                ELSE
                  PP(I,J,K) = PP(I,J,K+1) - RHO*GRAV*ZC(3,K)
                END IF
              END IF
  740       CONTINUE
          END IF
  730   CONTINUE
      END IF
C...MOKEI ( 2004.03 ) ...............................
         IF( LSTART.LT.0 ) THEN
            call flnam('.rsi')
            OPEN(IFLRI,FILE=trim(CFLNM),STATUS='OLD',
     $           FORM='UNFORMATTED',ERR=900)
            WRITE(LP,*) 'OPEN RESTART INPUT FILE'
            WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
            WRITE(LP,*) 'FILE NUMBER=',IFLRI
            READ(IFLRI,ERR=910,END=805) ISTEP1,TIME1,DT1,DTOLD1,
     $                                  LSURF1,LTURB1,LTEMP1,LCONC1
            WRITE(LP,*) '   READING ... STEP=',ISTEP1,' TIME=',TIME1
            WRITE(LP,*) '   INITIAL VELOCITY ( UU,VV,WW ) '
            READ(IFLRI,ERR=910) UU
            READ(IFLRI,ERR=910) VV
            READ(IFLRI,ERR=910) WW
            GO TO 806
  805       CONTINUE
            CALL ERRMSG('MKINIT',7033)
            WRITE(LP,*) 'INITIAL FILE ERROR ( MKINIT )'
            CALL ABORT1('')
  806       CONTINUE
          END IF
C............................................. END
C
C
C----------------------------------------------------------------------
C     (2) リスタートデータの読み込みを行う
C----------------------------------------------------------------------
      ELSE
C ...... *.str ファイルを読み込む
         IF( LOBST.EQ.1 ) THEN
           IF(IAUTOD.EQ.0)THEN
              READ(IFLST,END=321,ERR=920) ((HDEP(I,J),I=2,MXM),J=2,MYM)
              READ(IFLST,ERR=930) ((HH  (I,J),I=2,MXM),J=2,MYM)
              READ(IFLST,ERR=940) ((AMNG(I,J),I=2,MXM),J=2,MYM)
              IF( LSTOCDS.EQ.1 ) THEN
                 READ(IFLST,ERR=920) ((IDST(I,J),I=2,MXM),J=2,MYM)
                 READ(IFLST,ERR=920) ((HTDST1(I,J),I=2,MXM),J=2,MYM)
                 READ(IFLST,ERR=920) ((HTDST2(I,J),I=2,MXM),J=2,MYM)
              ENDIF
C
           ELSE
              CALL RDSUB2RB(HDEP,IFLST,0,IRTN)
              IF(IRTN.EQ.1) GOTO 920
              IF(IRTN.EQ.2) GOTO 321
              CALL RDSUB2RB(HH,IFLST,0,IRTN)
              IF(IRTN.EQ.1) GOTO 930
              CALL RDSUB2RB(AMNG,IFLST,0,IRTN)
              IF(IRTN.EQ.1) GOTO 940
              IF( LSTOCDS.EQ.1 ) THEN
                 CALL RDSUB2IB(IDST,IFLST,0,IRTN)
                 IF(IRTN.EQ.1) GOTO 920
                 CALL RDSUB2RB(HTDST1,IFLST,0,IRTN)
                 IF(IRTN.EQ.1) GOTO 920
                 CALL RDSUB2RB(HTDST2,IFLST,0,IRTN)
                 IF(IRTN.EQ.1) GOTO 920
              ENDIF
           ENDIF
           CLOSE(IFLST)
C
           DO 96 J=2,MYM
             IF (NSOMER(2).GT.0) HDEP(2    ,J)=HDEP(3    ,J)
             IF (NSOMER(3).GT.0) HDEP(MXM  ,J)=HDEP(MXM-1,J)
             IF (NSOMER(2).GT.0) AMNG(2    ,J)=AMNG(3    ,J)
             IF (NSOMER(3).GT.0) AMNG(MXM  ,J)=AMNG(MXM-1,J)
   96      CONTINUE
           DO 97 I=2,MXM
             IF (NSOMER(1).GT.0) HDEP(I,2    )=HDEP(I,3    )
             IF (NSOMER(4).GT.0) HDEP(I,MYM  )=HDEP(I,MYM-1)
             IF (NSOMER(1).GT.0) AMNG(I,2    )=AMNG(I,3    )
             IF (NSOMER(4).GT.0) AMNG(I,MYM  )=AMNG(I,MYM-1)
   97      CONTINUE
  321      CONTINUE
         END IF
C
         call flnam('.rsi')
         OPEN(IFLRI,FILE=trim(CFLNM),STATUS='OLD',
     $        FORM='UNFORMATTED',ERR=900)
         WRITE(LP,*) 'OPEN RESTART INPUT FILE'
         WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
         WRITE(LP,*) 'FILE NUMBER=',IFLRI
C
C ...... リスタートデータファイルの中から開始ステップ(時刻)のデータを検索する
         DO 800 N=1,1000000
            READ(IFLRI,ERR=910,END=810) ISTEP1,TIME1,DT1,DTOLD1,
     $                                  LSURF1,LTURB1,LTEMP1,LCONC1,
     $                                  LDUM1
            WRITE(LP,*) '   READING ... STEP=',ISTEP1,' TIME=',TIME1
            READ(IFLRI,ERR=910) UU
            READ(IFLRI,ERR=910) VV
            READ(IFLRI,ERR=910) WW
            READ(IFLRI,ERR=910) PP
            IF( LTEMP1.EQ.1 ) READ(IFLRI,ERR=910) TT
            IF( LCONC1.EQ.1 ) READ(IFLRI,ERR=910) CC
C ...... LTURB1=2(X1=AK,X2=EP),LTURB1=3(X1=Q2,X2=QL),LTURB1=4(X1=AK)
            IF( LTURB1.EQ.2.OR.LTURB1.EQ.3 ) READ(IFLRI,ERR=910) X1
            IF( LTURB1.EQ.2.OR.LTURB1.EQ.3 ) READ(IFLRI,ERR=910) X2
            IF( LTURB1.EQ.4 ) READ(IFLRI,ERR=910) X1
            IF( LSURF1.EQ.1 ) READ(IFLRI,ERR=910) FF
            IF( LSURF1.EQ.1 ) READ(IFLRI,ERR=910) HH
            IF( LSURF1.EQ.1 ) READ(IFLRI,ERR=910) KF
            IF( LSURF1.EQ.1 ) READ(IFLRI,ERR=910) KP
            IF( LSURF1.EQ.1 ) THEN
               IF( LSEDI.EQ.1 ) THEN
                  READ(IFLRI,ERR=910) VLEND
               ELSE
                  READ(IFLRI,ERR=910)
     $               (((VLEND(I,J,K),I=1,MX),J=1,MY),K=1,11)
               ENDIF
            ENDIF
            IF( LSURF1.EQ.1 ) READ(IFLRI,ERR=910) TMEND
            IF( LSURF1.EQ.1 ) READ(IFLRI,ERR=910) FREND
            IF( LSURF.EQ.1 ) READ(IFLRI,ERR=900) HDEP
            IF( LSURF.EQ.1.AND.IPARNT.GE.0 ) READ(IFLRI,ERR=910) HHBCN
            IF( LSURF.EQ.1.AND.IPARNT.GE.0 ) READ(IFLRI,ERR=910) UUBCN
            IF( LSURF.EQ.1.AND.IPARNT.GE.0 ) READ(IFLRI,ERR=910) VVBCN
C
C ......... 開始時刻とデータの時刻の比較
            DR1 = MAX(DT1*5.0D0,RSTART*1.0D-2)
            IF( LSTART.EQ.1 .AND. ISTEP.EQ.ISTEP1 ) GO TO 820
            IF( LSTART.EQ.2 .AND. ABS(RSTART-TIME1).LT.DR1 ) GO TO 820
  800    CONTINUE
  810    CONTINUE
C
C ...... リスタートファイル中に指定した時刻のデータが見つからなかった場合
         CALL ERRMSG('MKINIT',7034)
         WRITE(LP,*) 'SPECIFIED STEP/TIME IS NOT FOUND ',
     $               'IN RESTART FILE'
         WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
         CALL ABORT1('')
C
C ...... リスタートデータの読み込みに成功した場合
  820    CONTINUE
         WRITE(LP,*) 'RESTART STEP = ',ISTEP1
         WRITE(LP,*) 'RESTART TIME = ',TIME1
         ISTEP  = ISTEP1
         RSTART = TIME1
         TIME   = RSTART
         DT     = DT1
         DTOLD  = DTOLD1
C
C ...... 計算条件が異なる場合
         IF( LTEMP1.NE.LTEMP .OR. LCONC1.NE.LCONC .OR.
     $       LTURB1.NE.LTURB .OR. LSURF1.NE.LSURF ) THEN
            CALL ERRMSG('MKINIT',7035)
            WRITE(LP,*) 'CALCULATION CONDITION IS DIFFERENT FROM ',
     $                  'RESTART DATA'
            WRITE(LP,*) '        RESTART/INPUT'
            WRITE(LP,*) 'LTEMP = ',LTEMP1,'/',LTEMP
            WRITE(LP,*) 'LCONC = ',LCONC1,'/',LCONC
            WRITE(LP,*) 'LTURB = ',LTURB1,'/',LTURB
            WRITE(LP,*) 'LSURF = ',LSURF1,'/',LSURF
            CALL ABORT1('')
         END IF
      END IF
C
      DEALLOCATE(GVH,KDEP)
      RETURN
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('MKINIT',7036)
      WRITE(LP,*) 'FILE OPEN ERROR: RESTART INPUT FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLRI
      CALL ABORT1('')
C
  910 CONTINUE
      CALL ERRMSG('MKINIT',7037)
      WRITE(LP,*) 'READ ERROR: RESTART INPUT FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLRI
      CALL ABORT1('')
C324
C ... ファイル読み込みエラー
  920 CONTINUE
      CALL ERRMSG('MKINIT',7038)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=HDEP'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  930 CONTINUE
      CALL ERRMSG('MKINIT',7039)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=HH'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  940 CONTINUE
      CALL ERRMSG('MKINIT',7040)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=SD'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
      END
C
      SUBROUTINE DBWRXY2(AAA)
C======================================================================
C     tan精度実数型2次元配列をリスト出力する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
C     INCLUDE 'TIME.h'
C
      INTEGER,PARAMETER::NCOL1=11
C
      REAL(4),INTENT(INOUT)::AAA(MX,MY)
C
      CHARACTER(32)::FORM1='(1X,2H#=,I3                    )'
      CHARACTER(32)::FORM2='(1X,1H#,5X,1H|,  1000(I11:)    )'
      CHARACTER(32)::FORM3='(1X,7H------+,   1000(A11:)    )'
      CHARACTER(32)::FORM4='(1X,2H#=,I3,2H |,1000(1PE11.4:))'
C
      INTEGER::J,JE,JEND,JS,K,KE,KS,N,NBLOCK
C
      FORM2(7:7) = 'I'
      FORM4(7:7) = 'J'
      NBLOCK = ( MX-2 ) / NCOL1 + 1
      JEND   = MX
      KE     = MY
C
      JS = 1
      KS = 1
      JE = MIN( JS + NCOL1 - 1, JEND)
C
      DO 100 N=1,NBLOCK
         WRITE(LP,FORM2) (J,J=JS,JE)
         WRITE(LP,FORM3) ('----------+',J=JS,JE)
         DO 200 K=KE,KS,-1
            WRITE(LP,FORM4) K,(AAA(J,K),J=JS,JE)
  200    CONTINUE
         JS = JS + NCOL1
         JE = MIN( JE + NCOL1, JEND)
  100 CONTINUE
C
      RETURN
      END
