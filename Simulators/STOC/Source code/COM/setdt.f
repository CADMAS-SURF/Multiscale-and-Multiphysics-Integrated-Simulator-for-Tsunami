      SUBROUTINE SETDT(XC,YC,ZC,GV,GX,GY,GZ,UU,VV,WW,TMU,
     $                 INDP,INDU,INDV,INDW,IAD,JAD,KAD)
C======================================================================
C     時間刻み幅の自動設定
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'SEDIMENT.h'
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WW(MX,MY,MZ),TMU(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::IAD,JAD,KAD
C
      INTEGER::ICHK=0
C
      REAL(8)::ALEH,ALEV,DIF1,DT0,DT1,DT2,DTADV1
      REAL(8)::DTADVD,DTCND1,DTCNDD,DTCNDH,DTCNDV
      REAL(8)::DTDIF1,DTVIS1,DTVISD,DTVISH,DTVISV
      REAL(8)::DMUH,DMUV,DTDMUH,DTDMUV,DTDMUD
      REAL(8)::ENUH,ENUV,U0,U1,V0,V1,VAD,VELIJK,DTMU
      REAL(8)::VMAX1,VMIN1,W0,W1,XC1,YC1,ZC1
      INTEGER::I,IAL,II,IVL,IVS,IDF,J,JAL,JVL,JVS,JDF,K,KAL,KVL,KVS,KDF
C
C
      VMAX1  = - 1.0D0
      VMIN1  = 1.0D-20
      DTADV1 = 1.0D+20
      DTVIS1 = 1.0D+20
      DTCND1 = 1.0D+20
      DTDIF1 = 1.0D+20
C
C
C ... 対流項の制限
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            U0 = GX(I-1,J,K)*ABS(UU(I-1,J,K))*XC(6,I,J)
            U1 = GX(I  ,J,K)*ABS(UU(I  ,J,K))*XC(6,I,J)
            V0 = GY(I,J-1,K)*ABS(VV(I,J-1,K))*YC(6,J)
            V1 = GY(I,J  ,K)*ABS(VV(I,J  ,K))*YC(6,J)
            W0 = GZ(I,J,K-1)*ABS(WW(I,J,K-1))*ZC(6,K)
            W1 = GZ(I,J,K  )*ABS(WW(I,J,K  ))*ZC(6,K)
C
            DTADVD = GV(I,J,K)/MAX(U0,U1,V0,V1,W0,W1,VMIN1)
C
            IF(IDT.EQ.0) THEN
              IF(DTADVD.LT.DT.AND.GV(I,J,K).LT.1.0D0) THEN
                WRITE(16,61) I,J,K,GV(I,J,K),GX(I-1,J,K),GX(I,J,K),
     &                             GY(I,J-1,K),GY(I,J,K),DTADVD/DT
   61           FORMAT('I,J,K=',3I5,'  GV,GXM,GXI,GYM,GYJ,RATIO=',
     &                               1P,6D11.4)
              END IF
            END IF
C
            IF(DTADV1.GT.DTADVD) THEN
              IAD = I
              JAD = J
              KAD = K
              DTADV1 = DTADVD
              VAD = MAX(U0,U1,V0,V1,W0,W1,VMIN1)
            END IF
C
            VELIJK = DSQRT( (0.5D0*(UU(I-1,J,K)+UU(I,J,K)))**2
     1                    + (0.5D0*(VV(I,J-1,K)+VV(I,J,K)))**2
     2                    + (0.5D0*(WW(I,J,K-1)+WW(I,J,K)))**2 )
            IF(VELIJK.GT.VMAX1) THEN
              IVL = I
              JVL = J
              KVL = K
              VMAX1 = VELIJK
            ENDIF
         END IF
  100 CONTINUE
C
C
C ... 粘性項の制限(+k-ε拡散項)
C
      DTMU = 1.0D0
      IF(LTURB.EQ.2) THEN
        DTMU = MAX( DTMU,1.0/(MAX(SGK,SGE)) )
      END IF
C
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            XC1 = XC(6,I,J)
            YC1 = YC(6,J)
            ZC1 = ZC(6,K)
            IF( MXM .EQ. 2 ) XC1 = 0.0D0
            IF( MYM .EQ. 2 ) YC1 = 0.0D0
            IF( MZM .EQ. 2 ) ZC1 = 0.0D0
C
            ENUH = ANUH + TMU(I,J,K)*DTMU
            ENUV = ANUV + TMU(I,J,K)*DTMU
            IF(XC1.NE.0.D0 .OR. YC1.NE.0.D0) THEN
              DTVISH = 1.0D0/(2.0D0*ENUH*(XC1**2+YC1**2))
            ELSE
              DTVISH = 1.D20
            ENDIF
            IF(ZC1.NE.0.D0) THEN
              DTVISV = 1.0D0/(2.0D0*ENUV*ZC1**2)
            ELSE
              DTVISV = 1.D20
            ENDIF
            DTVISD = MIN(DTVISH,DTVISV)
            IF(DTVIS1.GT.DTVISD) THEN
              IVS = I
              JVS = J
              KVS = K
              DTVIS1 = DTVISD
            END IF
         END IF
  200 CONTINUE
C
C
C ... 熱伝導項の制限
C
      IF( LTEMP.EQ.1 ) THEN
         DO 300 K=2,MZM
         DO 300 J=2,MYM
         DO 300 I=2,MXM
            IF( INDP(I,J,K).GT.0 ) THEN
               XC1 = XC(6,I,J)
               YC1 = YC(6,J)
               ZC1 = ZC(6,K)
               IF( MXM .EQ. 2 ) XC1 = 0.0D0
               IF( MYM .EQ. 2 ) YC1 = 0.0D0
               IF( MZM .EQ. 2 ) ZC1 = 0.0D0
C
               ALEH = ALPH + TMU(I,J,K)/PRT
               ALEV = ALPV + TMU(I,J,K)/PRT
CC               DTCND1 = MIN( DTCND1,
CC     $            1.0D0/(2.0D0*ALE1*(XC1**2+YC1**2+ZC1**2)) )
               IF(XC1.NE.0.D0 .OR. YC1.NE.0.D0) THEN
                 DTCNDH = 1.0D0/(2.0D0*ALEH*(XC1**2+YC1**2))
               ELSE
                 DTCNDH = 1.D20
               ENDIF
               IF(ZC1.NE.0.D0.AND.IMVERT.EQ.0) THEN
                 DTCNDV = 1.0D0/(2.0D0*ALEV*ZC1**2)
               ELSE
                 DTCNDV = 1.D20
               ENDIF
               DTCNDD = MIN(DTCNDH,DTCNDV)
               IF(DTCND1.GT.DTCNDD) THEN
                 IAL = I
                 JAL = J
                 KAL = K
                 DTCND1 = DTCNDD
               END IF
            END IF
  300    CONTINUE
      END IF
C
C
C ... 濃度拡散項の制限
C
      IF( LCONC.EQ.1.OR.LSEDI.EQ.1 ) THEN
         DO 400 K=2,MZM
         DO 400 J=2,MYM
         DO 400 I=2,MXM
            IF( INDP(I,J,K).GT.0 ) THEN
               XC1 = XC(6,I,J)
               YC1 = YC(6,J)
               ZC1 = ZC(6,K)
               IF( MXM .EQ. 2 ) XC1 = 0.0D0
               IF( MYM .EQ. 2 ) YC1 = 0.0D0
               IF( MZM .EQ. 2 ) ZC1 = 0.0D0
C
               IF( LCONC.EQ.1.AND.LSEDI.EQ.1 ) THEN
                  DMUH = DIFH + TMU(I,J,K)/SCT
                  DMUV = DIFV + TMU(I,J,K)/SCT
                  DMUH = MAX(DIFHSD + TMU(I,J,K)/SCTHSD,DMUH)
                  DMUV = MAX(DIFVSD + TMU(I,J,K)/SCTVSD,DMUV)
               ELSEIF( LSEDI.EQ.1 ) THEN
                  DMUH = DIFHSD + TMU(I,J,K)/SCTHSD
                  DMUV = DIFVSD + TMU(I,J,K)/SCTVSD
               ELSE
                  DMUH = DIFH + TMU(I,J,K)/SCT
                  DMUV = DIFV + TMU(I,J,K)/SCT
               ENDIF
               IF(XC1.NE.0.D0 .OR. YC1.NE.0.D0) THEN
                 DTDMUH = 1.0D0/(2.0D0*DMUH*(XC1**2+YC1**2))
               ELSE
                 DTDMUH = 1.D20
               ENDIF
               IF(ZC1.NE.0.D0.AND.IMVERT.EQ.0) THEN
                 DTDMUV = 1.0D0/(2.0D0*DMUV*ZC1**2)
               ELSE
                 DTDMUV = 1.D20
               ENDIF
               DTDMUD = MIN(DTDMUH,DTDMUV)
               IF(DTDIF1.GT.DTDMUD) THEN
                 IDF = I
                 JDF = J
                 KDF = K
                 DTDIF1 = DTDMUD
               END IF
            END IF
  400    CONTINUE
      END IF
C
C
C ... 時間刻みに安全率と上下限を設定
C
      DTOLD = DT
C
      IF(IDT.EQ.0) THEN
        DT = DTCNST
      ELSE
        DT = DTSAFE * MIN( DTADV1, DTVIS1, DTCND1, DTDIF1 )
        IF(DT.LT.DTMIN) THEN
          I = IAD
          J = JAD
          K = KAD
          WRITE(6,61) I,J,K,GV(I,J,K),GX(I-1,J,K),GX(I,J,K),
     &                      GY(I,J-1,K),GY(I,J,K),DT/DTMIN
        END IF
        DT = MIN( MAX( DT, DTMIN ), DTMAX , DTOLD*1.2D0 )
      END IF
C
C
      IF(IDT.LT.0) THEN
        DT   = DTOLD
        DT0  = DTOLD*(DFLOAT(IDT)+0.1D0)
        IF(ISTEP.EQ.1.OR.DT0.GT.DTCNST) THEN
          DT1 = MIN( DTADV1, DTVIS1, DTCND1, DTDIF1 )
          II  = 1
 450      CONTINUE
          DT2 = DTCNST/(DFLOAT(II))
          IF(DT2.GT.DT1*DTSAFE) THEN
            II = II+2
            GO TO 450
          ELSE
            DT  = DT2
            IDT = 1
          END IF
        ELSE
          IDT = IDT+1
        END IF
      END IF
C
      IF(ICHK.NE.0) THEN
        IF(MOD(ISTEP,ICHK).EQ.0) THEN
          WRITE(43,500) TIME,DT,DTADV1,IAD,JAD,KAD,GV(IAD,JAD,KAD),
     1                  VAD,VMAX1,IVL,JVL,KVL
 500      FORMAT('# (DT-AUTO) TIME =',1PD12.5,'  DT =',1PD10.3,
     1           '  IDT =',I3,'  DTCNST=',1PD10.3
     2          /'  DTADV1 =',1PD10.3,'   (I,J,K)=',3I4,
     3                                '  GV =',1PD10.3
     4          /'  VMAX1  =',1PD10.3,'   (I,J,K)=',3I4)
        END IF
      END IF
C
      RETURN
      END
