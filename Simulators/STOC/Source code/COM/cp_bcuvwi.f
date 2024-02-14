      SUBROUTINE CP_BCUVWI(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                     INDU_NS,INDP_NS,
     1                     XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                     GX_ML,GX_NS,J_ML,K_ML,I_NS,J_NS,K_NS,
     3                     KF_ML,KG_ML,KF_NS,KG_NS,HDEP_ML,HDEP_NS,
     4                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HH_NS,
     $                     X1_ML,X2_ML,X1BCN,X2BCN,CSD_ML,CSDBCN,
     5                     UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,I1,I2,NN,
     6                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY_NS,
     7                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C-----------------------------------------------------------------------
C     境界流速(I=1,I=MXM)
C       LTURB=3 : X1=Q2,X2=QL
C       LTURB=4 : X1=AK
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'VVMAX.h'
C
      INTEGER,INTENT(INOUT)::I1,I2,NN
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
C
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDU_NS(MX_NS,MY_NS,MZ_NS),INDP_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML),
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GX_NS(MX_NS,MY_NS,MZ_NS)
C
      INTEGER,INTENT(INOUT)::
     $   J_ML(2,MY_ML),K_ML(2,MZ_ML),
     $   I_NS(2,MX_NS),J_NS(2,MY_NS),K_NS(2,MZ_NS)
      INTEGER,INTENT(INOUT)::
     $   KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   UU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   VV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   WW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   TT_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CSD_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   X1_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   X2_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::
     $   HH_NS(MX_NS,MY_NS),
     $   HDEP_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   UUBCN(MXY_NS,MZ_NS,4),VVBCN(MXY_NS,MZ_NS,4),
     $   WWBCN(MXY_NS,MZ_NS,4),
     $   TTBCN(MXY_NS,MZ_NS,4),CCBCN(MXY_NS,MZ_NS,4),
     $   CSDBCN(MXY_NS,MZ_NS,4),
     $   X1BCN(MXY_NS,MZ_NS,4),X2BCN(MXY_NS,MZ_NS,4)
C
      INTEGER::IDB=0
C
      REAL(8)::CSUM,DEP,DEPX,HH1,HZ1,HZ2,HZK,SS,SSUM,TSUM,UDIF
      REAL(8)::X1SUM,X2SUM
      REAL(8)::UEPS,USUM,VOL,VSUM,Y1,Y2,YJ,YM,YP,Z1,Z2,ZK,ZM,ZP
      REAL(8)::HNS1,HNS2
      INTEGER::I0,II,IXX,J,J1,J2,J3,J4,JJ,JJ1,JJ2,JM,JP,ISUM
      INTEGER::K,K1,K2,KF1,KG1,KG2,KK,KM,KP,N1,N2,N3,N4
C
      if(idb.ne.0) write(6,*) '# CP_BCUVWI  NN,MXY =',NN,MXY_NS
      IXX = 0
C
      II = I_NS(1,I1)
      I0 = 0
      IF(I1.EQ.1) I0=1
C
      DO 100 J=2,MY_NS-1
C
C     法線流速(U)
C
      JJ = J_NS(2,J)
      JM = JJ-1
      JP = JJ+1
      YJ = YC_ML(2,JJ)
      YM = YC_ML(2,JM)
      YP = YC_ML(2,JP)
C
      IF(YC_NS(2,J).GE.YJ) THEN
        Y1 = (YC_NS(2,J)-YJ)/(YP-YJ)
        Y2 = 1.0D0-Y1
        J1 = JP
        J2 = JJ
        J3 = JP
        J4 = JJ
      ELSE
        Y1 = (YC_NS(2,J)-YM)/(YJ-YM)
        Y2 = 1.0D0-Y1
        J1 = JJ
        J2 = JM
        J3 = JJ
        J4 = JM
      END IF
C
      N1 = J1
      N2 = J2
      N3 = J3
      N4 = J4
C
      DO 110 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 110
      J1 = N1
      J2 = N2
      J3 = N3
      J4 = N4
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      IF(J1.EQ.JP.AND.INDU_ML(II,JP,K1).LE.-2) J1=JJ
      IF(J3.EQ.JP.AND.INDU_ML(II,JP,K2).LE.-2) J3=JJ
      IF(J2.EQ.JM.AND.INDU_ML(II,JM,K1).LE.-2) J2=JJ
      IF(J4.EQ.JM.AND.INDU_ML(II,JM,K2).LE.-2) J4=JJ
C
      UUBCN(J,K,NN) = Y1*Z1*UU_ML(II,J1,K1)+Y1*Z2*UU_ML(II,J3,K2)
     1              + Y2*Z1*UU_ML(II,J2,K1)+Y2*Z2*UU_ML(II,J4,K2)
      IF(INDU_ML(II,JJ,KK).LE.-2) UUBCN(J,K,NN)=0.0D0
      if(idb.ne.0) write(6,*) '# j,k,uubcn =',j,k,uubcn(j,k,nn)
C
  110 CONTINUE
C
C     接線流速(V)
C
      JJ = J_NS(1,J)
      JM = JJ-1
      YJ = YC_ML(1,JJ)
      YM = YC_ML(1,JM)
      Y1 = (YC_NS(2,J)-YM)/(YJ-YM)
      Y2 = 1.0D0-Y1
      J1 = JJ
      J2 = JM
C
      DO 120 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 120
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      IF(K1.EQ.KP) THEN
        IF(INDV_ML(II  ,J1,K1).EQ.-4.OR.INDV_ML(II  ,J2,K1).EQ.-4.OR.
     1     INDV_ML(II+1,J1,K1).EQ.-4.OR.INDV_ML(II+1,J2,K1).EQ.-4) THEN
          K1 = KK
        END IF
      ELSE
        IF(INDV_ML(II  ,J1,K2).EQ.-4.OR.INDV_ML(II  ,J2,K2).EQ.-4.OR.
     1     INDV_ML(II+1,J1,K2).EQ.-4.OR.INDV_ML(II+1,J2,K2).EQ.-4) THEN
          K2 = KK
        END IF
      END IF
C
      VVBCN(J,K,NN) =
     1 Y1*Z1*(XC_ML(7,II)*VV_ML(II,J1,K1)+XC_ML(8,II)*VV_ML(II+1,J1,K1))
     2+Y1*Z2*(XC_ML(7,II)*VV_ML(II,J1,K2)+XC_ML(8,II)*VV_ML(II+1,J1,K2))
     3+Y2*Z1*(XC_ML(7,II)*VV_ML(II,J2,K1)+XC_ML(8,II)*VV_ML(II+1,J2,K1))
     4+Y2*Z2*(XC_ML(7,II)*VV_ML(II,J2,K2)+XC_ML(8,II)*VV_ML(II+1,J2,K2))
      if(idb.ne.0) write(6,*) '# j,k,vvbcn =',j,k,vvbcn(j,k,nn)
C
  120 CONTINUE
C
C     接線流速(W)
C
      JJ = J_NS(2,J)
      JM = JJ-1
      JP = JJ+1
      YJ = YC_ML(2,JJ)
      YM = YC_ML(2,JM)
      YP = YC_ML(2,JP)
C
      IF(YC_NS(2,J).GE.YJ) THEN
        Y1 = (YC_NS(2,J)-YJ)/(YP-YJ)
        Y2 = 1.0D0-Y1
        J1 = JP
        J2 = JJ
      ELSE
        Y1 = (YC_NS(2,J)-YM)/(YJ-YM)
        Y2 = 1.0D0-Y1
        J1 = JJ
        J2 = JM
      END IF
C
      N1 = J1
      N2 = J2
C
      DO 130 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 130
      J1 = N1
      J2 = N2
C
      KK = K_NS(1,K)
      KM = KK-1
      ZK = ZC_ML(1,KK)
      ZM = ZC_ML(1,KM)
      Z1 = (ZC_NS(2,K)-KM)/(ZK-ZM)
      Z2 = 1.0D0-Z1
      K1 = KK
      K2 = KM
C
      IF(J1.EQ.JP) THEN
        IF(INDW_ML(II  ,J1,K1).EQ.-4.OR.INDW_ML(II  ,J1,K2).EQ.-4.OR.
     1     INDW_ML(II+1,J1,K1).EQ.-4.OR.INDW_ML(II+1,J1,K2).EQ.-4) THEN
          J1 = JJ
        END IF
      ELSE
        IF(INDW_ML(II  ,J2,K1).EQ.-4.OR.INDW_ML(II  ,J2,K2).EQ.-4.OR.
     1     INDW_ML(II+1,J2,K1).EQ.-4.OR.INDW_ML(II+1,J2,K2).EQ.-4) THEN
          J2 = JJ
        END IF
      END IF
C
      WWBCN(J,K,NN) =
     1 Y1*Z1*(XC_ML(7,II)*WW_ML(II,J1,K1)+XC_ML(8,II)*WW_ML(II+1,J1,K1))
     2+Y1*Z2*(XC_ML(7,II)*WW_ML(II,J1,K2)+XC_ML(8,II)*WW_ML(II+1,J1,K2))
     3+Y2*Z1*(XC_ML(7,II)*WW_ML(II,J2,K1)+XC_ML(8,II)*WW_ML(II+1,J2,K1))
     4+Y2*Z2*(XC_ML(7,II)*WW_ML(II,J2,K2)+XC_ML(8,II)*WW_ML(II+1,J2,K2))
      if(idb.ne.0) write(6,*) '# j,k,wwbcn =',j,k,wwbcn(j,k,nn)
C
  130 CONTINUE
C
      IF(LTEMP.EQ.1) THEN
C     境界面温度(T)
C
      DO 140 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 140
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      VSUM = 0.0D0
      TSUM = 0.0D0
      IF(INDP_ML(II+1,J1,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II+1,J1,K1)
      END IF
      IF(INDP_ML(II+1,J1,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II+1,J1,K2)
      END IF
      IF(INDP_ML(II,J1,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II,J1,K1)
      END IF
      IF(INDP_ML(II,J1,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II,J1,K2)
      END IF
      IF(INDP_ML(II+1,J2,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II+1,J2,K1)
      END IF
      IF(INDP_ML(II+1,J2,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II+1,J2,K2)
      END IF
      IF(INDP_ML(II,J2,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II,J2,K1)
      END IF
      IF(INDP_ML(II,J2,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(II,J2,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        TTBCN(J,K,NN) = TSUM/VSUM
        if(idb.ne.0) write(6,*) '# j,k,ttbcn =',j,k,ttbcn(j,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWI',6210)
        WRITE(LP,610) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II+1,JJ,KK),
     $                I1,J ,K ,INDP_NS(I1,J ,K ),INDP_NS(I1+1,J ,K ),
     $                VSUM,TSUM
  610   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K) =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   CHILD  (I,J,K) =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   VSUM,TSUM      =',1P,D12.5)
        CALL ABORT1('')
      END IF
C
  140 CONTINUE
      END IF
C
      IF(LCONC.EQ.1) THEN
C     境界面濃度(C)
C
      DO 150 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 150
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      VSUM = 0.0D0
      CSUM = 0.0D0
      IF(INDP_ML(II+1,J1,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II+1,J1,K1)
      END IF
      IF(INDP_ML(II+1,J1,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II+1,J1,K2)
      END IF
      IF(INDP_ML(II,J1,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II,J1,K1)
      END IF
      IF(INDP_ML(II,J1,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II,J1,K2)
      END IF
      IF(INDP_ML(II+1,J2,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II+1,J2,K1)
      END IF
      IF(INDP_ML(II+1,J2,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II+1,J2,K2)
      END IF
      IF(INDP_ML(II,J2,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II,J2,K1)
      END IF
      IF(INDP_ML(II,J2,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(II,J2,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        CCBCN(J,K,NN) = CSUM/VSUM
        if(idb.ne.0) write(6,*) '# j,k,ccbcn =',j,k,ccbcn(j,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWI',6211)
        WRITE(LP,620) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II+1,JJ,KK),
     $                I1,J ,K ,INDP_NS(I1,J ,K ),INDP_NS(I1+1,J ,K ),
     $                VSUM,CSUM
  620   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K) =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   CHILD  (I,J,K) =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   VSUM,CSUM      =',1P,2D12.5)
        CALL ABORT1('')
      END IF
C
  150 CONTINUE
      END IF
C
      IF(LTURB.EQ.3) THEN
C     乱流量(X1,X2)
C
      DO 160 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 160
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      VSUM = 0.0D0
      X1SUM = 0.0D0
      X2SUM = 0.0D0
      IF(INDP_ML(II+1,J1,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J1,K1)
        X2SUM = X2SUM+VOL*X2_ML(II+1,J1,K1)
      END IF
      IF(INDP_ML(II+1,J1,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J1,K2)
        X2SUM = X2SUM+VOL*X2_ML(II+1,J1,K2)
      END IF
      IF(INDP_ML(II,J1,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J1,K1)
        X2SUM = X2SUM+VOL*X2_ML(II,J1,K1)
      END IF
      IF(INDP_ML(II,J1,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J1,K2)
        X2SUM = X2SUM+VOL*X2_ML(II,J1,K2)
      END IF
      IF(INDP_ML(II+1,J2,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J2,K1)
        X2SUM = X2SUM+VOL*X2_ML(II+1,J2,K1)
      END IF
      IF(INDP_ML(II+1,J2,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J2,K2)
        X2SUM = X2SUM+VOL*X2_ML(II+1,J2,K2)
      END IF
      IF(INDP_ML(II,J2,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J2,K1)
        X2SUM = X2SUM+VOL*X2_ML(II,J2,K1)
      END IF
      IF(INDP_ML(II,J2,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J2,K2)
        X2SUM = X2SUM+VOL*X2_ML(II,J2,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        X1BCN(J,K,NN) = X1SUM/VSUM
        X2BCN(J,K,NN) = X2SUM/VSUM
        if(idb.ne.0) write(6,*) '# j,k,ccbcn =',j,k,x1bcn(j,k,nn)
        if(idb.ne.0) write(6,*) '# j,k,ccbcn =',j,k,x2bcn(j,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWI',6212)
        WRITE(LP,650) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II+1,JJ,KK),
     $                I1,J ,K ,INDP_NS(I1,J ,K ),INDP_NS(I1+1,J ,K ),
     $                VSUM,X1SUM,X2SUM
  650   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K)  =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   CHILD  (I,J,K)  =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   VSUM,X1SUM,X2SUM=',1P,3D12.5)
        CALL ABORT1('')
      END IF
C
  160 CONTINUE
      END IF
C
      IF(LTURB.EQ.4) THEN
C     乱流量(X1,X2)
C
      DO 170 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 170
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      VSUM = 0.0D0
      X1SUM = 0.0D0
      IF(INDP_ML(II+1,J1,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J1,K1)
      END IF
      IF(INDP_ML(II+1,J1,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J1,K2)
      END IF
      IF(INDP_ML(II,J1,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J1,K1)
      END IF
      IF(INDP_ML(II,J1,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J1,K2)
      END IF
      IF(INDP_ML(II+1,J2,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J2,K1)
      END IF
      IF(INDP_ML(II+1,J2,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II+1,J2,K2)
      END IF
      IF(INDP_ML(II,J2,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J2,K1)
      END IF
      IF(INDP_ML(II,J2,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(II,J2,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        X1BCN(J,K,NN) = X1SUM/VSUM
        if(idb.ne.0) write(6,*) '# j,k,ccbcn =',j,k,x1bcn(j,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWI',6213)
        WRITE(LP,640) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II+1,JJ,KK),
     $                I1,J ,K ,INDP_NS(I1,J ,K ),INDP_NS(I1+1,J ,K ),
     $                VSUM,X1SUM
  640   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K)  =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   CHILD  (I,J,K)  =',3I5,'   INDP(I),(I+1) =',2I5
     $        /'   VSUM,X1SUM      =',1P,2D12.5)
        CALL ABORT1('')
      END IF
C
  170 CONTINUE
      END IF
C
      IF(LSEDI.EQ.1) THEN
C     境界面浮遊砂濃度(CSEDI)
C
      DO 180 K=KG_NS(I2,J),KF_NS(I2,J)
      IF(INDU_NS(I1,J,K).NE.-1) GO TO 180
C
      KK = K_NS(2,K)
      KM = KK-1
      KP = KK+1
      ZK = ZC_ML(2,KK)
      ZM = ZC_ML(2,KM)
      ZP = ZC_ML(2,KP)
C
      IF(ZC_NS(2,K).GE.ZK) THEN
        Z1 = (ZC_NS(2,K)-ZK)/(ZP-ZK)
        Z2 = 1.0D0-Z1
        K1 = KP
        K2 = KK
        IF(KK.EQ.KF_ML(II+I0,JJ)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II+I0,JJ)) K2=KK
      END IF
C
      VSUM = 0.0D0
      CSUM = 0.0D0
      IF(INDP_ML(II+1,J1,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II+1,J1,K1)
      END IF
      IF(INDP_ML(II+1,J1,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II+1,J1,K2)
      END IF
      IF(INDP_ML(II,J1,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II,J1,K1)
      END IF
      IF(INDP_ML(II,J1,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II,J1,K2)
      END IF
      IF(INDP_ML(II+1,J2,K1).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II+1,J2,K1)
      END IF
      IF(INDP_ML(II+1,J2,K2).EQ.1) THEN
        VOL = (XC_ML(1,II)-XC_ML(2,II))*Y2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II+1,J2,K2)
      END IF
      IF(INDP_ML(II,J2,K1).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II,J2,K1)
      END IF
      IF(INDP_ML(II,J2,K2).EQ.1) THEN
        VOL = (XC_ML(2,II+1)-XC_ML(1,II))*Y2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(II,J2,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        CSDBCN(J,K,NN) = CSUM/VSUM
        if(idb.ne.0) write(6,*) '# j,k,csdbcn =',j,k,csdbcn(j,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWI',6214)
        WRITE(LP,620) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II+1,JJ,KK),
     $                I1,J ,K ,INDP_NS(I1,J ,K ),INDP_NS(I1+1,J ,K ),
     $                VSUM,CSUM
        CALL ABORT1('')
      END IF
C
  180 CONTINUE
      END IF
C
  100 CONTINUE
C
C     法線方向流量の修正 GX_ML*SS_ML*UU_ML=SUM(GX_NS*SS_NS*UUBCN)
C
      JJ1 = J_NS(2,2)
      JJ2 = J_NS(2,MY_NS-1)
      DO 200 J=JJ1,JJ2
        KG1 = KG_ML(II+I0,J)
        KF1 = MAX(KF_ML(II,J),KF_ML(II+1,J))
        IF(KF1.EQ.MZ_ML) GO TO 200
        J1 = J_ML(1,J-1)+1
        J2 = J_ML(1,J)
C ..... 自由表面メッシュ以下
        DO 210 K=KG1,KF1-1
          IF(INDU_ML(II,J,K).LE.-2) GO TO 210
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          USUM = 0.0D0
          SSUM = 0.0D0
          DO 220 JJ=J1,J2
            DO 230 KK=K1,K2
              IF(INDU_NS(I1,JJ,KK).GE.-1) THEN
                SS = GX_NS(I1,JJ,KK)*YC_NS(4,JJ)*ZC_NS(4,KK)
                SSUM = SSUM+SS
                USUM = USUM+SS*UUBCN(JJ,KK,NN)
              END IF
  230       CONTINUE
  220     CONTINUE
C
          UDIF = GX_ML(II,J,K)*YC_ML(4,J)*ZC_ML(4,K)*UU_ML(II,J,K)
     1         - USUM
          IF(SSUM.GT.0.0D0) THEN
            UEPS = UDIF/SSUM
            DO 240 JJ=J1,J2
              DO 250 KK=K1,K2
                UUBCN(JJ,KK,NN) = UUBCN(JJ,KK,NN)+UEPS
  250         CONTINUE
  240       CONTINUE
          ELSE
            CALL ERRMSG('CP_BCUVWI',6215)
            WRITE(LP,*) '### INDEX ERROR SSUM=0.0 STOP CP_BCUVWJ ###'
            WRITE(LP,*) '    I_ML,J_ML,K =',II,J,K
            CALL ABORT1('')
          END IF
C
  210   CONTINUE
C ..... 自由表面メッシュ
        DO 215 K=KF1,KF1
          IF(INDU_ML(II,J,K).LE.-2) GO TO 215
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          USUM = 0.0D0
          SSUM = 0.0D0
          DEP = HDEP_ML(II,J)*XC_ML(7,II)+HDEP_ML(II+1,J)*XC_ML(8,II)
          DEPX = ZC_ML(1,K)-GX_ML(II,J,K)*ZC_ML(4,K)
          IF(DEPX.GT.DEP) DEP=DEPX
          DO 225 JJ=J1,J2
            HNS1 = HH_NS(I1,JJ)
            IF(HNS1.GT.1.0D10) HNS1=HDEP_NS(I1,JJ)
            HNS2 = HH_NS(I1+1,JJ)
            IF(HNS2.GT.1.0D10) HNS2=HDEP_NS(I1+1,JJ)
            HH1 = HNS1*XC_NS(7,I1)+HNS2*XC_NS(8,I1)
            DO 235 KK=K1,K2
              IF(KK.GT.KF_NS(I1+I0,JJ)) GO TO 235
              IF(KK.LT.KG_NS(I1+I0,JJ)) UUBCN(JJ,KK,NN)=0.0D0
              IF(KK.LT.KG_NS(I1+I0,JJ)) GO TO 235
              IF(INDU_NS(I1,JJ,KK).GE.-1) THEN
                HZ1 = MIN(ZC_NS(1,KK),HH1)
                HZ2 = MAX(ZC_NS(1,KK-1),DEP)
C
                IF(K.EQ.KG1.AND.KK.EQ.KG_NS(I1+I0,JJ)) THEN
                  IF(HH_NS(I1+I0,JJ)-HDEP_NS(I1+I0,JJ).LE.GXB) THEN
                    IF(NN.EQ.2) THEN
                      IF(HH_NS(I1,JJ)-HDEP_ML(II,J).GT.GXB) THEN
                        IF(HH_NS(I1,JJ)-HDEP_NS(I1+1,JJ).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(I1,JJ)-HDEP_ML(II,J).LE.GXB.AND.
     $                   UUBCN(JJ,KK,NN).GE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    ELSE IF(NN.EQ.3) THEN
                      IF(HH_NS(I1+1,JJ)-HDEP_ML(II+1,J).GT.GXB) THEN
                        IF(HH_NS(I1+1,JJ)-HDEP_NS(I1,JJ).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(I1+1,JJ)-HDEP_ML(II+1,J).LE.GXB.AND.
     $                   UUBCN(JJ,KK,NN).LE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    END IF
                  ELSE
                    IF(NN.EQ.2) THEN
                      IF(HH_NS(I1,JJ)-HDEP_ML(II,J).LE.GXB) THEN
                        IF(HH_NS(I1+1,JJ)-HDEP_ML(II,J).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(I1,JJ)-HDEP_NS(I1+1,JJ).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(I1+1,JJ)-HDEP_ML(II,J).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    ELSE IF(NN.EQ.3) THEN
                      IF(HH_NS(I1+1,JJ)-HDEP_ML(II+1,J).LE.GXB) THEN
                        IF(HH_NS(I1,JJ)-HDEP_ML(II+1,J).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(I1,JJ)-HDEP_ML(II+1,J).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(I1+1,JJ)-HDEP_NS(I1,JJ).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    END IF
                  END IF
                END IF
C
                SS  = YC_NS(4,JJ)*(HZ1-HZ2)
                IF(HZ1-HZ2.LT.GXB) THEN
                  SS = 0.0D0
                  UUBCN(JJ,KK,NN) = 0.0D0
                END IF
                IF(KK.NE.KG_NS(I1+I0,JJ)) SS=SS*GX_NS(I1,JJ,KK)
                SSUM = SSUM+SS
                USUM = USUM+SS*UUBCN(JJ,KK,NN)
              END IF
  235       CONTINUE
  225     CONTINUE
C
          HZ1 = HH_ML(II,J)*XC_ML(7,II)+HH_ML(II+1,J)*XC_ML(8,II)
          IF(IXX.EQ.0) THEN
            HZ2 = MAX(ZC_ML(1,K-1),DEP)
          ELSE
            KG2 = KG_NS(I1+I0,J1)
            HZK = MAX(ZC_NS(1,KG2-1),HDEP_NS(I1+I0,J1))
            HZ2 = MAX(ZC_ML(1,K-1),DEP,HZK)
          END IF
          IF(HZ1-HDEP_ML(II  ,J).LE.GXB.OR.
     $       HZ1-HDEP_ML(II+1,J).LE.GXB) HZ2=HZ1
          SS  = YC_ML(4,J)*(HZ1-HZ2)
          IF(K.NE.KG_ML(II+I0,J)) SS=SS*GX_ML(II,J,K)
          UDIF = SS*UU_ML(II,J,K)-USUM
          IF(HZ1-HZ2.GE.GXB) THEN
            ISUM = 0
            IF(SSUM.EQ.0.0D0) THEN
              ISUM = 1
              UEPS= 0.0D0
            ELSE
              UEPS = UDIF/SSUM
            END IF
            DO 245 JJ=J1,J2
              HNS1 = HH_NS(I1,JJ)
              IF(HNS1.GT.1.0D10) HNS1=HDEP_NS(I1,JJ)
              HNS2 = HH_NS(I1+1,JJ)
              IF(HNS2.GT.1.0D10) HNS2=HDEP_NS(I1+1,JJ)
              HH1 = HNS1*XC_NS(7,I1)+HNS2*XC_NS(8,I1)
              DO 255 KK=K1,K2
                IF(KK.GT.KF_NS(I1+I0,JJ)) GO TO 255
                IF(KK.LT.KG_NS(I1+I0,JJ)) GO TO 255
                IF(INDU_NS(I1,JJ,KK).LT.-1) GO TO 255
                HZ1 = MIN(ZC_NS(1,KK),HH1)
                HZ2 = MAX(ZC_NS(1,KK-1),DEP)
C
                IF(K.EQ.KG1.AND.KK.EQ.KG_NS(I1+I0,JJ)) THEN
                  IF(HH_NS(I1+I0,JJ)-HDEP_NS(I1+I0,JJ).LE.GXB) THEN
                    IF(NN.EQ.2) THEN
                      IF(HH_NS(I1,JJ)-HDEP_ML(II,J).GT.GXB) THEN
                        IF(HH_NS(I1,JJ)-HDEP_NS(I1+1,JJ).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(I1,JJ)-HDEP_ML(II,J).LE.GXB.AND.
     $                   UUBCN(JJ,KK,NN).GE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    ELSE IF(NN.EQ.3) THEN
                      IF(HH_NS(I1+1,JJ)-HDEP_ML(II+1,J).GT.GXB) THEN
                        IF(HH_NS(I1+1,JJ)-HDEP_NS(I1,JJ).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(I1+1,JJ)-HDEP_ML(II+1,J).LE.GXB.AND.
     $                   UUBCN(JJ,KK,NN).LE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    END IF
C
                    IF(ISUM.NE.0) THEN
                      WRITE(6,630) NN,ISUM,HNS1,HNS2,HH1,
     $                             HDEP_NS(I1,JJ),HDEP_NS(I1+1,JJ),DEP
  630                 FORMAT('NN,ISUM=',2I3,'  HHI,HHP,HH1=',1P,3D12.5,
     $                                      '  DPI,DPP,DEP=',3D12.5)
                    END IF
                  ELSE
                    IF(NN.EQ.2) THEN
                      IF(HH_NS(I1,JJ)-HDEP_ML(II,J).LE.GXB) THEN
                        IF(HH_NS(I1+1,JJ)-HDEP_ML(II,J).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(I1,JJ)-HDEP_NS(I1+1,JJ).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(I1+1,JJ)-HDEP_ML(II,J).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    ELSE IF(NN.EQ.3) THEN
                      IF(HH_NS(I1+1,JJ)-HDEP_ML(II+1,J).LE.GXB) THEN
                        IF(HH_NS(I1,JJ)-HDEP_ML(II+1,J).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(UUBCN(JJ,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(I1,JJ)-HDEP_ML(II+1,J).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(I1+1,JJ)-HDEP_NS(I1,JJ).LE.GXB.AND.
     $                     UUBCN(JJ,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    END IF
C
                    IF(ISUM.NE.0) THEN
                      ISUM = 2
                      WRITE(6,630) NN,ISUM,HNS1,HNS2,HH1,
     $                             HDEP_NS(I1,JJ),HDEP_NS(I1+1,JJ),DEP
                    END IF
                  END IF
                END IF
C
                IF(HZ1-HZ2.GE.GXB) THEN
                  UUBCN(JJ,KK,NN) = UUBCN(JJ,KK,NN)+UEPS
                  IF(ABS(UUBCN(JJ,KK,NN)).GT.VVMAX) THEN
                    WRITE(LP,600) ISTEP,TIME,II,J,K,UU_ML(II,J,K),
     $                            JJ,KK,NN,UUBCN(JJ,KK,NN)
 600                FORMAT('ISTEP,TIME=',I7,1P,D12.5,'  PARENT(I,J,K)=',
     $                     3I5,'  UU=',1P,D12.5,'  CHILD(JJ,KK,NN)=',
     $                     3I5,'  UUBCN=',1PD12.5)
                    UUBCN(JJ,KK,NN) = SIGN(VVMAX,UUBCN(JJ,KK,NN))
                  END IF
                END IF
  255         CONTINUE
  245       CONTINUE
          ELSE
            DO 265 JJ=J1,J2
              DO 275 KK=K1,K2
                UUBCN(JJ,KK,NN) = 0.0D0
  275         CONTINUE
  265       CONTINUE
          END IF
C
  215   CONTINUE
C
  200   CONTINUE
C
      RETURN
      END
