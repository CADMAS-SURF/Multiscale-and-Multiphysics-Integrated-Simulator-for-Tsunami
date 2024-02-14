      SUBROUTINE UPUVWP(UU,VV,WW,PP,DP,HH,PATM,GV,GV0,GVD,CMD,XC,YC,ZC,
     $                  INDP,INDU,INDV,INDW,KF,KG,KP)
C======================================================================
C     流速と圧力を更新する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),DP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(IN)   ::GV(MX,MY,MZ),GV0(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::GVD(MX,MY,MZ),CMD(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY)
C
      REAL(8)::DP1,PMIN,GV1,GVCM0,GVCM1
      INTEGER::I,IXX,J,K,KF1,KG1,KP1
C
C
C----------------------------------------------------------------------
C     (1) 圧力の更新
C----------------------------------------------------------------------
      PMIN=1.0D30
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
c         DO 110 K=KG(I,J),KP(I,J)
         IF( INDP(I,J,K).GT.0 ) THEN
            IF(K.GT.KF(I,J)) THEN
               PP(I,J,K) = 0.0D0
            ELSE
               PP(I,J,K) = PP(I,J,K) + DP(I,J,K)
               PMIN = MIN(PMIN,PP(I,J,K))
            END IF
         END IF
c  110    CONTINUE
  100 CONTINUE
C
      IF(LSURF.EQ.0) THEN
C .... 2004.06.04 ......
        PMIN = 0.0D0
C ......................
        DO 150 K=2,MZM
        DO 150 J=2,MYM
        DO 150 I=2,MXM
          IF( INDP(I,J,K).GT.0 ) THEN
            PP(I,J,K) = PP(I,J,K)-PMIN
          END IF
  150   CONTINUE
C
      ELSE
C
C ..... 表面セルの上(KP1+1)にも圧力を補間しておく ( 2005.01.25 )
C
C FOR EXAMPLE JET
        IXX = 1
C ORIGINAL
C       IXX = 0
C
        IF(IXX.NE.0) THEN
        DO 160 J=2,MYM
        DO 160 I=2,MXM
          IF(KF(I,J).EQ.MZ) GO TO 160
          KP1 = KP(I,J)
          IF(INDP(I,J,KP1).GT.0.AND.INDP(I,J,KP1+1).GT.0) THEN
            IF(HH(I,J).NE.ZC(2,KP1)) THEN
              PP(I,J,KP1+1) = PP(I,J,KP1)
     $        + (PATM(I,J)-PP(I,J,KP1))/(HH(I,J)-ZC(2,KP1))*ZC(3,KP1)
            ELSE
              PP(I,J,KP1+1) = PP(I,J,KP1)+RHO*GRAV*ZC(3,KP1)
            END IF
          END IF
 160    CONTINUE
        END IF
C
      END IF
C
C
C----------------------------------------------------------------------
C     (2) 流速の更新
C----------------------------------------------------------------------
      DO 200 J=2,MYM
      DO 200 I=1,MXM
C
         KG1 = MIN(KG(I,J),KG(I+1,J))
         KP1 = MAX(KP(I,J),KP(I+1,J))
         KF1 = MAX(KF(I,J),KF(I+1,J))
         DO 210 K=KG1,KP1-1
            IF( INDU(I,J,K).GT.0 ) THEN
               DP1 = ( DP(I+1,J,K) - DP(I,J,K) ) * XC(5,I,J)
            GVCM0=GV0(I,J,K)*(GVD(I,J,K)+(1.0D0-GVD(I,J,K))*CMD(I,J,K))
            GVCM1=GV0(I+1,J,K)*(GVD(I+1,J,K)
     $    +(1.0D0-GVD(I+1,J,K))*CMD(I+1,J,K))
               GV1 = ( GV(I,J,K)*XC(7,I,J) + GV(I+1,J,K)*XC(8,I,J))
     $             / ( GVCM0*XC(7,I,J) + GVCM1*XC(8,I,J))
               UU(I,J,K) = UU(I,J,K) - GV1*DTV / RHO * DP1
            ELSE IF( INDU(I,J,K).EQ.0 ) THEN
               IF( INDP(I,J,K).GT.0 ) THEN
                 DP1 = - DP(I,J,K)*2.0D0*XC(6,I,J)
               ELSE
                 DP1 = DP(I+1,J,K)*2.0D0*XC(6,I+1,J)
               END IF
               UU(I,J,K) = UU(I,J,K) - DTV / RHO * DP1
            END IF
  210    CONTINUE
C
         DO 220 K=KP1,KF1-1
            IF( INDU(I,J,K).GT.0 ) THEN
               IF( K.GT.KF(I+1,J) ) THEN
                 DP1 = - DP(I,J,K)*XC(6,I,J)
               ELSE IF( K.GT.KF(I,J) ) THEN
                 DP1 = DP(I+1,J,K)*XC(6,I+1,J)
               ELSE
                 DP1 = (DP(I+1,J,K)-DP(I,J,K))*XC(5,I,J)
               END IF
            GVCM0=GV0(I,J,K)*(GVD(I,J,K)+(1.0D0-GVD(I,J,K))*CMD(I,J,K))
            GVCM1=GV0(I+1,J,K)*(GVD(I+1,J,K)
     $    +(1.0D0-GVD(I+1,J,K))*CMD(I+1,J,K))
               GV1 = ( GV(I,J,K)*XC(7,I,J) + GV(I+1,J,K)*XC(8,I,J))
     $             / ( GVCM0*XC(7,I,J) + GVCM1*XC(8,I,J))
               UU(I,J,K) = UU(I,J,K) - GV1*DTV / RHO * DP1
            END IF
  220    CONTINUE
  200 CONTINUE
C
      DO 300 J=1,MYM
      DO 300 I=2,MXM
C
         KG1 = MIN(KG(I,J),KG(I,J+1))
         KP1 = MAX(KP(I,J),KP(I,J+1))
         KF1 = MAX(KF(I,J),KF(I,J+1))
         DO 310 K=KG1,KP1-1
            IF( INDV(I,J,K).GT.0 ) THEN
               DP1 = ( DP(I,J+1,K) - DP(I,J,K) ) * YC(5,J)
            GVCM0=GV0(I,J,K)*(GVD(I,J,K)+(1.0D0-GVD(I,J,K))*CMD(I,J,K))
            GVCM1=GV0(I,J+1,K)*(GVD(I,J+1,K)
     $    +(1.0D0-GVD(I,J+1,K))*CMD(I,J+1,K))
               GV1 = ( GV(I,J,K)*YC(7,J) + GV(I,J+1,K)*YC(8,J))
     $             / ( GVCM0*YC(7,J) + GVCM1*YC(8,J))
               VV(I,J,K) = VV(I,J,K) - GV1*DTV / RHO * DP1
            ELSE IF( INDV(I,J,K).EQ.0 ) THEN
               IF( INDP(I,J,K).GT.0 ) THEN
                 DP1 = - DP(I,J,K)*2.0D0*YC(6,J)
               ELSE
                 DP1 = DP(I,J+1,K)*2.0D0*YC(6,J+1)
               END IF
               VV(I,J,K) = VV(I,J,K) - DTV / RHO * DP1
            END IF
  310    CONTINUE
C
         DO 320 K=KP1,KF1-1
            IF( INDV(I,J,K).GT.0 ) THEN
               IF( K.GT.KF(I,J+1) ) THEN
                 DP1 = - DP(I,J,K)*YC(6,J)
               ELSE IF( K.GT.KF(I,J) ) THEN
                 DP1 = DP(I,J+1,K)*YC(6,J+1)
               ELSE
                 DP1 = (DP(I,J+1,K)-DP(I,J,K))*YC(5,J)
               END IF
            GVCM0=GV0(I,J,K)*(GVD(I,J,K)+(1.0D0-GVD(I,J,K))*CMD(I,J,K))
            GVCM1=GV0(I,J+1,K)*(GVD(I,J+1,K)
     $    +(1.0D0-GVD(I,J+1,K))*CMD(I,J+1,K))
               GV1 = ( GV(I,J,K)*YC(7,J) + GV(I,J+1,K)*YC(8,J))
     $             / ( GVCM0*YC(7,J) + GVCM1*YC(8,J))
               VV(I,J,K) = VV(I,J,K) - GV1*DTV / RHO * DP1
            END IF
  320    CONTINUE
  300 CONTINUE
C
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C
         DO 410 K=KG(I,J)-1,KF(I,J)-1
            IF( INDW(I,J,K).GT.0 ) THEN
C               DP1 = ( DP(I,J,K+1) - DP(I,J,K) ) * ZC(6,K)
               DP1 = ( DP(I,J,K+1) - DP(I,J,K) ) * ZC(5,K)
            GVCM0=GV0(I,J,K)*(GVD(I,J,K)+(1.0D0-GVD(I,J,K))*CMD(I,J,K))
            GVCM1=GV0(I,J,K+1)*(GVD(I,J,K+1)
     $    +(1.0D0-GVD(I,J,K+1))*CMD(I,J,K+1))
               GV1 = ( GV(I,J,K)*ZC(7,K) + GV(I,J,K+1)*ZC(8,K))
     $             / ( GVCM0*ZC(7,K) + GVCM1*ZC(8,K))
               WW(I,J,K) = WW(I,J,K) - GV1*DTV / RHO * DP1
C.....
            ELSE IF( INDW(I,J,K).EQ.0 ) THEN
               IF( INDP(I,J,K).GT.0 ) THEN
                 DP1 = - DP(I,J,K)*2.0D0*ZC(6,K)
               ELSE
C                 DP1 = DP(I,J,K)*2.0D0*ZC(6,K+1)
                 DP1 = DP(I,J,K+1)*2.0D0*ZC(6,K+1)
               END IF
               WW(I,J,K) = WW(I,J,K) - DTV / RHO * DP1
C.....
            END IF
  410    CONTINUE
  400 CONTINUE
C
      RETURN
      END
