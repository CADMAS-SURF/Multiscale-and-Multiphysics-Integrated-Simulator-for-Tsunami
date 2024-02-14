      SUBROUTINE CLFALLW(FALLWX,FALLWY,FALLWZ,DHX,DHY,CFALLWX,CFALLWY,
     $                   DFALLWNX,DFALLWNY,DFALLWTX,DFALLWTY,
     $                   HU,HV,UU,VV,HH,FF,GV0,GX0,GY0,XC,YC,ZC,
     $                   INDU,INDV,LLWALB,KF,KG,WRKXU,WRKXW,WRKYV,WRKYW)
C======================================================================
C     落水モデルの計算を行う
C     適用位置は、cldpルーチンで圧力(水面)を補正している位置
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      include 'TIMEI.h'
      include 'FILE.h'
      include 'FILEC.h'
C
      REAL(8),INTENT(OUT)::FALLWX(MX,MY),FALLWY(MX,MY),FALLWZ(MX,MY)
      REAL(8),INTENT(OUT)::DHX(MX,MY),DHY(MX,MY)
      REAL(8),INTENT(INOUT)::CFALLWX(MX,MY),CFALLWY(MX,MY)
      REAL(8),INTENT(INOUT)::DFALLWNX(MX,MY),DFALLWNY(MX,MY)
      REAL(8),INTENT(INOUT)::DFALLWTX(MX,MY),DFALLWTY(MX,MY)
      REAL(8),INTENT(IN)::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(IN)::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(IN)::HH(MX,MY)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV0(MX,MY,MZ),GX0(MX,MY,MZ),GY0(MX,MY,MZ)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8)::WRKXU(MX,MY,MZ),WRKXW(MX,MY,MZ)
      REAL(8)::WRKYV(MX,MY,MZ),WRKYW(MX,MY,MZ)
C
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(IN)::LLWALB(3,MLWALB),KF(MX,MY),KG(MX,MY)
C
      INTEGER::I,J,K,N,K1,K2,ID
      REAL(8)::H1,MOF,UOF,VOF,WOF,DZ,GX1,GY1,HH0,HH1,HH2
C
      INTEGER:: INITFLG
      DATA INITFLG /0/
C
      IF( INITFLG.EQ.0 .AND. JFALLW.EQ.1 ) THEN
         call flnam('.fwc')
         write(lp,*) trim(CFLNM)
         OPEN(IFLFW,FILE=trim(CFLNM),STATUS='OLD',
     $        FORM='FORMATTED',ERR=900)
         READ(IFLFW,*,ERR=901)
         READ(IFLFW,*,ERR=901) ((CFALLWX(I,J),I=1,MXM),J=2,MYM)
         READ(IFLFW,*,ERR=901)
         READ(IFLFW,*,ERR=901) ((DFALLWNX(I,J),I=1,MXM),J=2,MYM)
         READ(IFLFW,*,ERR=901)
         READ(IFLFW,*,ERR=901) ((DFALLWTX(I,J),I=1,MXM),J=2,MYM)
         READ(IFLFW,*,ERR=901)
         READ(IFLFW,*,ERR=901) ((CFALLWY(I,J),I=2,MXM),J=1,MYM)
         READ(IFLFW,*,ERR=901)
         READ(IFLFW,*,ERR=901) ((DFALLWNY(I,J),I=2,MXM),J=1,MYM)
         READ(IFLFW,*,ERR=901)
         READ(IFLFW,*,ERR=901) ((DFALLWTY(I,J),I=2,MXM),J=1,MYM)
         CLOSE(IFLFW)
      ENDIF
      INITFLG=1
C
C
C----------------------------------------------------------------------
C     (1) X方向の落水位置で運動量を計算。作業用配列WRKXU,WRKXWに格納する
C         WRKXU,WRKXW定義位置：計算セルのX面
C----------------------------------------------------------------------
      WRKXU=0.0D0
      WRKXW=0.0D0
C
      DO 100 J=2,MYM
      DO 100 I=1,MXM
         IF( KF(I,J).EQ.KF(I+1,J) ) CYCLE
C
         IF(KF(I,J).GT.KF(I+1,J)) THEN  ! 左から右へ
            ID=1
            K1=KF(I+1,J)
            K2=KF(I  ,J)
            H1=HH(I+1,J)
         ELSE                           ! 右から左へ
            ID=-1
            K1=KF(I  ,J)
            K2=KF(I+1,J)
            H1=HH(I  ,J)
         ENDIF
C
         DO 110 K=K1+1,K2
            IF( INDU(I,J,K).GT.0 ) THEN
               IF( ID*HU(I,J,K).GT.0.0D0) THEN
                  MOF = ID*HU(I,J,K)*ZC(4,K)
                  UOF = ID*UU(I,J,K)
                  DZ  = ZC(1,K-1)-H1
                  WOF = SQRT(2.0D0*ABS(GRAV)*DZ)
                  WRKXU(I,J,K)=ID*CFALLWX(I,J)*MOF*UOF
                  WRKXW(I,J,K)=ID*CFALLWX(I,J)*MOF*WOF
               ENDIF
            ENDIF
  110    CONTINUE
  100 CONTINUE
C
C
C ...... 防潮堤用処理
!CDIR NODEP
      DO 120 N=1,MLWALBX
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GX1 = 1.0D0-GX0(I,J,K)
C
         IF(     FF(I,J,K).GT.GX1.AND.FF(I+1,J,K).LT.GX1 ) THEN ! 左から右へ
            IF( HU(I,J,K).GT.0.0D0) THEN
               MOF = HU(I,J,K)*ZC(4,K)
               UOF = UU(I,J,K)
               DZ  = ZC(1,K-1)+GX1*ZC(4,K)-HH(I+1,J)
               WOF = SQRT(2.0D0*ABS(GRAV)*DZ)
               WRKXU(I,J,K)=CFALLWX(I,J)*MOF*UOF
               WRKXW(I,J,K)=CFALLWX(I,J)*MOF*WOF
            ENDIF
         ELSEIF( FF(I,J,K).LT.GX1.AND.FF(I+1,J,K).GT.GX1 ) THEN ! 右から左へ
            IF( HU(I,J,K).LT.0.0D0) THEN
               MOF = -HU(I,J,K)*ZC(4,K)
               UOF = -UU(I,J,K)
               DZ  = ZC(1,K-1)+GX1*ZC(4,K)-HH(I  ,J)
               WOF = SQRT(2.0D0*ABS(GRAV)*DZ)
               WRKXU(I,J,K)=-CFALLWX(I,J)*MOF*UOF
               WRKXW(I,J,K)=-CFALLWX(I,J)*MOF*WOF
            ENDIF
         ENDIF
  120 CONTINUE
C
C
C----------------------------------------------------------------------
C     (2) Y方向の落水位置で運動量を計算。作業用配列WRKYV,WRKYWに格納する
C         WRKYV,WRKYW定義位置：計算セルのY面
C----------------------------------------------------------------------
      WRKYV=0.0D0
      WRKYW=0.0D0
C
      DO 200 J=1,MYM
      DO 200 I=2,MXM
         IF( KF(I,J).EQ.KF(I,J+1) ) CYCLE
C
         IF(KF(I,J).GT.KF(I,J+1)) THEN  ! 下から上へ
            ID=1
            K1=KF(I,J+1)
            K2=KF(I,J  )
            H1=HH(I,J+1)
         ELSE                           ! 上から下へ
            ID=-1
            K1=KF(I,J  )
            K2=KF(I,J+1)
            H1=HH(I,J  )
         ENDIF
C
         DO 210 K=K1+1,K2
            IF( INDV(I,J,K).GT.0 ) THEN
               IF( ID*HV(I,J,K).GT.0.0D0) THEN
                  MOF = ID*HV(I,J,K)*ZC(4,K)
                  VOF = ID*VV(I,J,K)
                  DZ  = ZC(1,K-1)-H1
                  WOF = SQRT(2.0D0*ABS(GRAV)*DZ)
                  WRKYV(I,J,K)=ID*CFALLWY(I,J)*MOF*VOF
                  WRKYW(I,J,K)=ID*CFALLWY(I,J)*MOF*WOF
               ENDIF
            ENDIF
  210    CONTINUE
  200 CONTINUE
C
C ...... 防潮堤用処理
!CDIR NODEP
      DO 220 N=MLWALBX+1,MLWALB
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GY1 = 1.0D0-GY0(I,J,K)
C
         IF(     FF(I,J,K).GT.GY1.AND.FF(I,J+1,K).LT.GY1 ) THEN ! 左から右へ
            IF( HV(I,J,K).GT.0.0D0) THEN
               MOF = HV(I,J,K)*ZC(4,K)
               VOF = VV(I,J,K)
               DZ  = ZC(1,K-1)+GY1*ZC(4,K)-HH(I,J+1)
               WOF = SQRT(2.0D0*ABS(GRAV)*DZ)
               WRKYV(I,J,K)=CFALLWY(I,J)*MOF*VOF
               WRKYW(I,J,K)=CFALLWY(I,J)*MOF*WOF
            ENDIF
         ELSEIF( FF(I,J,K).LT.GY1.AND.FF(I,J+1,K).GT.GY1 ) THEN ! 右から左へ
            IF( HV(I,J,K).LT.0.0D0) THEN
               MOF = -HV(I,J,K)*ZC(4,K)
               VOF = -VV(I,J,K)
               DZ  = ZC(1,K-1)+GY1*ZC(4,K)-HH(I,J  )
               WOF = SQRT(2.0D0*ABS(GRAV)*DZ)
               WRKYV(I,J,K)=-CFALLWY(I,J)*MOF*VOF
               WRKYW(I,J,K)=-CFALLWY(I,J)*MOF*WOF
            ENDIF
         ENDIF
  220 CONTINUE
C
C
C----------------------------------------------------------------------
C     (3) 落水の運動量を与える位置で、配列FALLWX,FALLWY,FALLWZの値を設定する
C         FALLWX,FALLWY定義位置：計算セルのX,Y面
C         FALLWZ定義位置       ：計算セル中心
C----------------------------------------------------------------------
      FALLWX=0.0D0
      FALLWY=0.0D0
      FALLWZ=0.0D0
      DHX=0.0D0
      DHY=0.0D0
C
      DO 300 J=2,MYM
      DO 300 I=1,MXM
         DO 310 K=2,MZM
            IF( INDU(I,J,K).GT.0 ) THEN
               HH1 = MAX(FF(I  ,J,K)-1.0D0+GV0(I  ,J,K),0.0D0)
               HH2 = MAX(FF(I+1,J,K)-1.0D0+GV0(I+1,J,K),0.0D0)
               HH0 = HH1*XC(8,I,J) + HH2*XC(7,I,J)
               DHX(I,J)=DHX(I,J)+HH0*ZC(4,K)
            ENDIF
C
C ......... 水平方向運動量
            IF( WRKXU(I,J,K).GT.0.0D0 .AND. I.LT.MXM ) THEN
               FALLWX(I+1,J)=FALLWX(I+1,J)+WRKXU(I,J,K)
            ELSEIF( WRKXU(I,J,K).LT.0.0D0 .AND. I.GT.1 ) THEN
               FALLWX(I-1,J)=FALLWX(I-1,J)+WRKXU(I,J,K)
            ENDIF
C
C ......... 鉛直方向運動量
            IF( WRKXW(I,J,K).GT.0.0D0 ) THEN
               IF( MLNS.EQ.1.AND.KF(I+1,J).GT.KG(I+1,J) ) THEN
                  FALLWZ(I+1,J)=FALLWZ(I+1,J)-WRKXW(I,J,K)
               ELSE
                  FALLWX(I+1,J  )
     $               =FALLWX(I+1,J  )+DFALLWNX(I,J)*WRKXW(I,J,K)
                  FALLWY(I+1,J-1)
     $               =FALLWY(I+1,J-1)-DFALLWTX(I,J)*WRKXW(I,J,K)
                  FALLWY(I+1,J  )
     $               =FALLWY(I+1,J  )+DFALLWTX(I,J)*WRKXW(I,J,K)
               ENDIF
            ELSEIF( WRKXW(I,J,K).LT.0.0D0 ) THEN
               IF( MLNS.EQ.1.AND.KF(I  ,J).GT.KG(I  ,J) ) THEN
                  FALLWZ(I  ,J)=FALLWZ(I  ,J)+WRKXW(I,J,K)
               ELSE
                  FALLWX(I-1,J  )
     $               =FALLWX(I-1,J  )+DFALLWNX(I,J)*WRKXW(I,J,K)
                  FALLWY(I  ,J-1)
     $               =FALLWY(I  ,J-1)+DFALLWTX(I,J)*WRKXW(I,J,K)
                  FALLWY(I  ,J  )
     $               =FALLWY(I  ,J  )-DFALLWTX(I,J)*WRKXW(I,J,K)
               ENDIF
            ENDIF
  310    CONTINUE
  300 CONTINUE
C
      DO 400 J=1,MYM
      DO 400 I=2,MXM
         DO 410 K=2,MZM
            IF( INDV(I,J,K).GT.0 ) THEN
               HH1 = MAX(FF(I,J  ,K)-1.0D0+GV0(I,J  ,K),0.0D0)
               HH2 = MAX(FF(I,J+1,K)-1.0D0+GV0(I,J+1,K),0.0D0)
               HH0 = HH1*YC(8,J) + HH2*YC(7,J)
               DHY(I,J)=DHY(I,J)+HH0*ZC(4,K)
            ENDIF
C
C ......... 水平方向運動量
            IF( WRKYV(I,J,K).GT.0.0D0 .AND. J.LT.MYM ) THEN
               FALLWY(I,J+1)=FALLWY(I,J+1)+WRKYV(I,J,K)
            ELSEIF( WRKYV(I,J,K).LT.0.0D0 .AND. J.GT.1 ) THEN
               FALLWY(I,J-1)=FALLWY(I,J-1)+WRKYV(I,J,K)
            ENDIF
C
C ......... 鉛直方向運動量
            IF( WRKYW(I,J,K).GT.0.0D0 ) THEN
               IF( MLNS.EQ.1.AND.KF(I,J+1).GT.KG(I,J+1) ) THEN
                  FALLWZ(I,J+1)=FALLWZ(I,J+1)-WRKYW(I,J,K)
               ELSE
                  FALLWY(I  ,J+1)
     $               =FALLWY(I  ,J+1)+DFALLWNY(I,J)*WRKYW(I,J,K)
                  FALLWX(I-1,J+1)
     $               =FALLWX(I-1,J+1)-DFALLWTY(I,J)*WRKYW(I,J,K)
                  FALLWX(I  ,J+1)
     $               =FALLWX(I  ,J+1)+DFALLWTY(I,J)*WRKYW(I,J,K)
               ENDIF
            ELSEIF( WRKYW(I,J,K).LT.0.0D0 ) THEN
               IF( MLNS.EQ.1.AND.KF(I,J  ).GT.KG(I,J  ) ) THEN
                  FALLWZ(I,J  )=FALLWZ(I,J  )+WRKYW(I,J,K)
               ELSE
                  FALLWY(I  ,J-1)
     $               =FALLWY(I  ,J-1)+DFALLWNY(I,J)*WRKYW(I,J,K)
                  FALLWX(I-1,J  )
     $               =FALLWX(I-1,J  )+DFALLWTY(I,J)*WRKYW(I,J,K)
                  FALLWX(I  ,J  )
     $               =FALLWX(I  ,J  )-DFALLWTY(I,J)*WRKYW(I,J,K)
               ENDIF
            ENDIF
  410    CONTINUE
  400 CONTINUE
C
      RETURN
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('CLFALLW',6850)
      WRITE(LP,*) 'FILE OPEN ERROR: FWC FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLFW
      CALL ABORT1('')
C
  901 CONTINUE
      CALL ERRMSG('CLFALLW',6851)
      WRITE(LP,*) 'FILE READ ERROR: FWC FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLFW
      CALL ABORT1('')
      END
