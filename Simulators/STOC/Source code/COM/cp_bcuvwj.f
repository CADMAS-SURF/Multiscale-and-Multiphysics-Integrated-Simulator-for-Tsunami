      SUBROUTINE CP_BCUVWJ(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                     INDV_NS,INDP_NS,
     1                     XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                     GY_ML,GY_NS,I_ML,K_ML,I_NS,J_NS,K_NS,
     3                     KF_ML,KG_ML,KF_NS,KG_NS,HDEP_ML,HDEP_NS,
     4                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HH_NS,
     $                     X1_ML,X2_ML,X1BCN,X2BCN,CSD_ML,CSDBCN,
     5                     UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,J1,J2,NN,
     6                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY_NS,
     7                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C-----------------------------------------------------------------------
C     境界流速(J=1,J=MYM)
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
      INTEGER,INTENT(INOUT)::J1,J2,NN
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
C
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_NS(MX_NS,MY_NS,MZ_NS),INDP_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML),
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      REAL(8),INTENT(INOUT)::
     $   GY_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GY_NS(MX_NS,MY_NS,MZ_NS)
C
      INTEGER,INTENT(INOUT)::
     $   I_ML(2,MX_ML),K_ML(2,MZ_ML),
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
      REAL(8)::CSUM,DEP,DEPY,HH1,HZ1,HZ2,HZK,SS,SSUM,TSUM,VDIF
      REAL(8)::X1SUM,X2SUM
      REAL(8)::VEPS,VOL,VSUM,X1,X2,XI,XM,XP,Z1,Z2,ZK,ZM,ZP
      REAL(8)::HNS1,HNS2
      INTEGER::I,I1,I2,I3,I4,IER,II,II1,II2,IM,IP,IXX,J,J0,JJ,ISUM
      INTEGER::K,K1,K2,KF1,KG1,KG2,KK,KM,KP,N1,N2,N3,N4
C
      if(idb.ne.0) write(6,*) '# CP_BCUVWJ  NN,MXY =',NN,MXY_NS
      IXX = 0
C
      JJ = J_NS(1,J1)
      J0 = 0
      IF(J1.EQ.1) J0=1
C
      DO 100 I=2,MX_NS-1
C
C     接線流速 U(I,J1,K)
C
      II = I_NS(1,I)
      IM = II-1
      XI = XC_ML(1,II)
      XM = XC_ML(1,IM)
      X1 = (XC_NS(2,I)-XM)/(XI-XM)
      X2 = 1.0D0-X1
      I1 = II
      I2 = IM
C
      DO 110 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 110
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      IF(K1.EQ.KP) THEN
        IF(INDU_ML(I1,JJ  ,K1).EQ.-4.OR.INDU_ML(I2,JJ  ,K1).EQ.-4.OR.
     1     INDU_ML(I1,JJ+1,K1).EQ.-4.OR.INDU_ML(I2,JJ+1,K1).EQ.-4) THEN
          K1 = KK
        END IF
      ELSE
        IF(INDU_ML(I1,JJ  ,K2).EQ.-4.OR.INDU_ML(I2,JJ  ,K2).EQ.-4.OR.
     1     INDU_ML(I1,JJ+1,K2).EQ.-4.OR.INDU_ML(I2,JJ+1,K2).EQ.-4) THEN
          K2 = KK
        END IF
      END IF
C
      UUBCN(I,K,NN) =
     1 X1*Z1*(YC_ML(7,JJ)*UU_ML(I1,JJ,K1)+YC_ML(8,JJ)*UU_ML(I1,JJ+1,K1))
     2+X1*Z2*(YC_ML(7,JJ)*UU_ML(I1,JJ,K2)+YC_ML(8,JJ)*UU_ML(I1,JJ+1,K2))
     3+X2*Z1*(YC_ML(7,JJ)*UU_ML(I2,JJ,K1)+YC_ML(8,JJ)*UU_ML(I2,JJ+1,K1))
     4+X2*Z2*(YC_ML(7,JJ)*UU_ML(I2,JJ,K2)+YC_ML(8,JJ)*UU_ML(I2,JJ+1,K2))
      if(idb.ne.0) write(6,*) '# i,k,uubcn =',i,k,uubcn(i,k,nn)
C
  110 CONTINUE
C
C     法線流速(V)
C
      II = I_NS(2,I)
      IM = II-1
      IP = II+1
      XI = XC_ML(2,II)
      XM = XC_ML(2,IM)
      XP = XC_ML(2,IP)
C
      IF(XC_NS(2,I).GE.XI) THEN
        X1 = (XC_NS(2,I)-XI)/(XP-XI)
        X2 = 1.0D0-X1
        I1 = IP
        I2 = II
        I3 = IP
        I4 = II
      ELSE
        X1 = (XC_NS(2,I)-XM)/(XI-XM)
        X2 = 1.0D0-X1
        I1 = II
        I2 = IM
        I3 = II
        I4 = IM
      END IF
C
      N1 = I1
      N2 = I2
      N3 = I3
      N4 = I4
C
      DO 120 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 120
      I1 = N1
      I2 = N2
      I3 = N3
      I4 = N4
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      IF(I1.EQ.IP.AND.INDV_ML(IP,JJ,K1).LE.-2) I1=II
      IF(I3.EQ.IP.AND.INDV_ML(IP,JJ,K2).LE.-2) I3=II
      IF(I2.EQ.IM.AND.INDV_ML(IM,JJ,K1).LE.-2) I2=II
      IF(I4.EQ.IM.AND.INDV_ML(IM,JJ,K2).LE.-2) I4=II
C
      VVBCN(I,K,NN) = X1*Z1*VV_ML(I1,JJ,K1)+X1*Z2*VV_ML(I1,JJ,K2)
     1              + X2*Z1*VV_ML(I2,JJ,K1)+X2*Z2*VV_ML(I2,JJ,K2)
      IF(INDV_ML(II,JJ,KK).LE.-2) VVBCN(I,K,NN)=0.0D0
      if(idb.ne.0) write(6,*) '# i,k,vvbcn =',i,k,vvbcn(i,k,nn)
C
  120 CONTINUE
C
C     接線流速(W)
C
      II = I_NS(2,I)
      IM = II-1
      IP = II+1
      XI = XC_ML(2,II)
      XM = XC_ML(2,IM)
      XP = XC_ML(2,IP)
C
      IF(XC_NS(2,I).GE.XI) THEN
        X1 = (XC_NS(2,I)-XI)/(XP-XI)
        X2 = 1.0D0-X1
        I1 = IP
        I2 = II
      ELSE
        X1 = (XC_NS(2,I)-XM)/(XI-XM)
        X2 = 1.0D0-X1
        I1 = II
        I2 = IM
      END IF
C
      N1 = I1
      N2 = I2
C
      DO 130 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 130
      I1 = N1
      I2 = N2
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
      IF(I1.EQ.IP) THEN
        IF(INDW_ML(I1,JJ  ,K1).EQ.-4.OR.INDW_ML(I1,JJ  ,K2).EQ.-4.OR.
     1     INDW_ML(I1,JJ+1,K1).EQ.-4.OR.INDW_ML(I1,JJ+1,K2).EQ.-4) THEN
          I1 = II
        END IF
      ELSE
        IF(INDW_ML(I2,JJ  ,K1).EQ.-4.OR.INDW_ML(I2,JJ  ,K2).EQ.-4.OR.
     1     INDW_ML(I2,JJ+1,K1).EQ.-4.OR.INDW_ML(I2,JJ+1,K2).EQ.-4) THEN
          I2 = II
        END IF
      END IF
C
      WWBCN(I,K,NN) =
     1 X1*Z1*(YC_ML(7,JJ)*WW_ML(I1,JJ,K1)+YC_ML(8,JJ)*WW_ML(I1,JJ+1,K1))
     2+X1*Z2*(YC_ML(7,JJ)*WW_ML(I1,JJ,K2)+YC_ML(8,JJ)*WW_ML(I1,JJ+1,K2))
     3+X2*Z1*(YC_ML(7,JJ)*WW_ML(I2,JJ,K1)+YC_ML(8,JJ)*WW_ML(I2,JJ+1,K1))
     4+X2*Z2*(YC_ML(7,JJ)*WW_ML(I2,JJ,K2)+YC_ML(8,JJ)*WW_ML(I2,JJ+1,K2))
      if(idb.ne.0) write(6,*) '# i,k,wwbcn =',i,k,wwbcn(i,k,nn)
C
  130 CONTINUE
C
      IF(LTEMP.EQ.1) THEN
C     境界面温度(T)
C
      DO 140 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 140
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      VSUM = 0.0D0
      TSUM = 0.0D0
      IF(INDP_ML(I1,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I1,JJ+1,K1)
      END IF
      IF(INDP_ML(I1,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I1,JJ+1,K2)
      END IF
      IF(INDP_ML(I1,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I1,JJ,K1)
      END IF
      IF(INDP_ML(I1,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I1,JJ,K2)
      END IF
      IF(INDP_ML(I2,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I2,JJ+1,K1)
      END IF
      IF(INDP_ML(I2,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I2,JJ+1,K2)
      END IF
      IF(INDP_ML(I2,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z1
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I2,JJ,K1)
      END IF
      IF(INDP_ML(I2,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z2
        VSUM = VSUM+VOL
        TSUM = TSUM+VOL*TT_ML(I2,JJ,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        TTBCN(I,K,NN) = TSUM/VSUM
        if(idb.ne.0) write(6,*) '# i,k,ttbcn =',i,k,ttbcn(i,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWJ',6220)
        WRITE(LP,610) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II,JJ+1,KK),
     $                I ,J1,K ,INDP_NS(I ,J1,K ),INDP_NS(I ,J1+1,K ),
     $                VSUM,TSUM
  610   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K) =',3I5,'   INDP(I),(J+1) =',2I5
     $        /'   CHILD  (I,J,K) =',3I5,'   INDP(I),(J+1) =',2I5
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
      DO 150 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 150
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      VSUM = 0.0D0
      CSUM = 0.0D0
      IF(INDP_ML(I1,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I1,JJ+1,K1)
      END IF
      IF(INDP_ML(I1,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I1,JJ+1,K2)
      END IF
      IF(INDP_ML(I1,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I1,JJ,K1)
      END IF
      IF(INDP_ML(I1,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I1,JJ,K2)
      END IF
      IF(INDP_ML(I2,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I2,JJ+1,K1)
      END IF
      IF(INDP_ML(I2,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I2,JJ+1,K2)
      END IF
      IF(INDP_ML(I2,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I2,JJ,K1)
      END IF
      IF(INDP_ML(I2,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CC_ML(I2,JJ,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        CCBCN(I,K,NN) = CSUM/VSUM
        if(idb.ne.0) write(6,*) '# i,k,ccbcn =',i,k,ccbcn(i,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWJ',6221)
        WRITE(LP,620) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II,JJ+1,KK),
     $                I ,J1,K ,INDP_NS(I ,J1,K ),INDP_NS(I ,J1+1,K ),
     $                VSUM,CSUM
  620   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K) =',3I5,'   INDP(I),(J+1) =',2I5
     $        /'   CHILD  (I,J,K) =',3I5,'   INDP(I),(J+1) =',2I5
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
      DO 160 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 160
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      VSUM = 0.0D0
      X1SUM = 0.0D0
      X2SUM = 0.0D0
      IF(INDP_ML(I1,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ+1,K1)
        X2SUM = X2SUM+VOL*X2_ML(I1,JJ+1,K1)
      END IF
      IF(INDP_ML(I1,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ+1,K2)
        X2SUM = X2SUM+VOL*X2_ML(I1,JJ+1,K2)
      END IF
      IF(INDP_ML(I1,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ,K1)
        X2SUM = X2SUM+VOL*X2_ML(I1,JJ,K1)
      END IF
      IF(INDP_ML(I1,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ,K2)
        X2SUM = X2SUM+VOL*X2_ML(I1,JJ,K2)
      END IF
      IF(INDP_ML(I2,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ+1,K1)
        X2SUM = X2SUM+VOL*X2_ML(I2,JJ+1,K1)
      END IF
      IF(INDP_ML(I2,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ+1,K2)
        X2SUM = X2SUM+VOL*X2_ML(I2,JJ+1,K2)
      END IF
      IF(INDP_ML(I2,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ,K1)
        X2SUM = X2SUM+VOL*X2_ML(I2,JJ,K1)
      END IF
      IF(INDP_ML(I2,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ,K2)
        X2SUM = X2SUM+VOL*X2_ML(I2,JJ,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        X1BCN(I,K,NN) = X1SUM/VSUM
        X2BCN(I,K,NN) = X2SUM/VSUM
        if(idb.ne.0) write(6,*) '# i,k,x1bcn =',i,k,x1bcn(i,k,nn)
        if(idb.ne.0) write(6,*) '# i,k,x2bcn =',i,k,x2bcn(i,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWJ',6222)
        WRITE(LP,650) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II,JJ+1,KK),
     $                I ,J1,K ,INDP_NS(I ,J1,K ),INDP_NS(I ,J1+1,K ),
     $                VSUM,X1SUM,X2SUM
  650   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K)  =',3I5,'   INDP(I),(J+1) =',2I5
     $        /'   CHILD  (I,J,K)  =',3I5,'   INDP(I),(J+1) =',2I5
     $        /'   VSUM,X1SUM,X2SUM=',1P,3D12.5)
        CALL ABORT1('')
      END IF
C
  160 CONTINUE
      END IF
C
      IF(LTURB.EQ.4) THEN
C     乱流量(X1)
C
      DO 170 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 170
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      VSUM = 0.0D0
      X1SUM = 0.0D0
      IF(INDP_ML(I1,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ+1,K1)
      END IF
      IF(INDP_ML(I1,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ+1,K2)
      END IF
      IF(INDP_ML(I1,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ,K1)
      END IF
      IF(INDP_ML(I1,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I1,JJ,K2)
      END IF
      IF(INDP_ML(I2,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ+1,K1)
      END IF
      IF(INDP_ML(I2,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ+1,K2)
      END IF
      IF(INDP_ML(I2,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z1
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ,K1)
      END IF
      IF(INDP_ML(I2,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z2
        VSUM = VSUM+VOL
        X1SUM = X1SUM+VOL*X1_ML(I2,JJ,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        X1BCN(I,K,NN) = X1SUM/VSUM
        if(idb.ne.0) write(6,*) '# i,k,x1bcn =',i,k,x1bcn(i,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWJ',6223)
        WRITE(LP,640) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II,JJ+1,KK),
     $                I ,J1,K ,INDP_NS(I ,J1,K ),INDP_NS(I ,J1+1,K ),
     $                VSUM,X1SUM
  640   FORMAT('## INTERPOLATION ERROR ##'
     $        /'   PARENT (I,J,K)  =',3I5,'   INDP(I),(J+1) =',2I5
     $        /'   CHILD  (I,J,K)  =',3I5,'   INDP(I),(J+1) =',2I5
     $        /'   VSUM,X1SUM      =',1P,2D12.5)
        CALL ABORT1('')
      END IF
C
  170 CONTINUE
      END IF
C
      IF(LSEDI.EQ.1) THEN
C     境界面浮遊砂濃度(C)
C
      DO 180 K=KG_NS(I,J2),KF_NS(I,J2)
      IF(INDV_NS(I,J1,K).NE.-1) GO TO 180
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
        IF(KK.EQ.KF_ML(II,JJ+J0)) K1=KK
      ELSE
        Z1 = (ZC_NS(2,K)-ZM)/(ZK-ZM)
        Z2 = 1.0D0-Z1
        K1 = KK
        K2 = KM
        IF(KK.EQ.KG_ML(II,JJ+J0)) K2=KK
      END IF
C
      VSUM = 0.0D0
      CSUM = 0.0D0
      IF(INDP_ML(I1,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I1,JJ+1,K1)
      END IF
      IF(INDP_ML(I1,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I1,JJ+1,K2)
      END IF
      IF(INDP_ML(I1,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I1,JJ,K1)
      END IF
      IF(INDP_ML(I1,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X1*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I1,JJ,K2)
      END IF
      IF(INDP_ML(I2,JJ+1,K1).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I2,JJ+1,K1)
      END IF
      IF(INDP_ML(I2,JJ+1,K2).EQ.1) THEN
        VOL = (YC_ML(1,JJ)-YC_ML(2,JJ))*X2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I2,JJ+1,K2)
      END IF
      IF(INDP_ML(I2,JJ,K1).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z1
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I2,JJ,K1)
      END IF
      IF(INDP_ML(I2,JJ,K2).EQ.1) THEN
        VOL = (YC_ML(2,JJ+1)-YC_ML(1,JJ))*X2*Z2
        VSUM = VSUM+VOL
        CSUM = CSUM+VOL*CSD_ML(I2,JJ,K2)
      END IF
C
      IF(VSUM.GT.0.0) THEN
        CSDBCN(I,K,NN) = CSUM/VSUM
        if(idb.ne.0) write(6,*) '# i,k,csdbcn =',i,k,csdbcn(i,k,nn)
      ELSE
        CALL ERRMSG('CP_BCUVWJ',6224)
        WRITE(LP,620) II,JJ,KK,INDP_ML(II,JJ,KK),INDP_ML(II,JJ+1,KK),
     $                I ,J1,K ,INDP_NS(I ,J1,K ),INDP_NS(I ,J1+1,K ),
     $                VSUM,CSUM
        CALL ABORT1('')
      END IF
C
  180 CONTINUE
      END IF
C
  100 CONTINUE
C
C     法線方向流量の修正 GY_ML*SS_ML*VV_ML=SUM(GY_NS*SS_NS*VVBCN)
C
      II1 = I_NS(2,2)
      II2 = I_NS(2,MX_NS-1)
      DO 200 I=II1,II2
        KG1 = KG_ML(I,JJ+J0)
        KF1 = MAX(KF_ML(I,JJ),KF_ML(I,JJ+1))
        IF(KF1.EQ.MZ_ML) GO TO 200
        I1 = I_ML(1,I-1)+1
        I2 = I_ML(1,I)
C ..... 自由表面メッシュ以下
        DO 210 K=KG1,KF1-1
          IF(INDV_ML(I,JJ,K).LE.-2) GO TO 210
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          VSUM = 0.0D0
          SSUM = 0.0D0
          DO 220 II=I1,I2
            DO 230 KK=K1,K2
              IF(INDV_NS(II,J1,KK).GE.-1) THEN
                SS = GY_NS(II,J1,KK)*XC_NS(4,II)*ZC_NS(4,KK)
                SSUM = SSUM+SS
                VSUM = VSUM+SS*VVBCN(II,KK,NN)
              END IF
  230       CONTINUE
  220     CONTINUE
C
          VDIF = GY_ML(I,JJ,K)*XC_ML(4,I)*ZC_ML(4,K)*VV_ML(I,JJ,K)
     1         - VSUM
          IF(SSUM.GT.0.0D0) THEN
            VEPS = VDIF/SSUM
            DO 240 II=I1,I2
              DO 250 KK=K1,K2
                VVBCN(II,KK,NN) = VVBCN(II,KK,NN)+VEPS
  250         CONTINUE
  240       CONTINUE
          ELSE
            CALL ERRMSG('CP_BCUVWJ',6225)
            WRITE(LP,*) '### INDEX ERROR SSUM=0.0 STOP CP_BCUVWJ ###'
            WRITE(LP,*) '    I_ML,J_ML,K =',I,JJ,K
            CALL ABORT1('')
          END IF
C
  210   CONTINUE
C ..... 自由表面メッシュ
        DO 215 K=KF1,KF1
          IF(INDV_ML(I,JJ,K).LE.-2) GO TO 215
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          VSUM = 0.0D0
          SSUM = 0.0D0
          DEP = HDEP_ML(I,JJ)*YC_ML(7,JJ)+HDEP_ML(I,JJ+1)*YC_ML(8,JJ)
          DEPY = ZC_ML(1,K)-GY_ML(I,JJ,K)*ZC_ML(4,K)
          IF(DEPY.GT.DEP) DEP=DEPY
          DO 225 II=I1,I2
            HNS1 = HH_NS(II,J1)
            IF(HNS1.GT.1.0D10) HNS1=HDEP_NS(II,J1)
            HNS2 = HH_NS(II,J1+1)
            IF(HNS2.GT.1.0D10) HNS2=HDEP_NS(II,J1+1)
            HH1 = HNS1*YC_NS(7,J1)+HNS2*YC_NS(8,J1)
            DO 235 KK=K1,K2
              IF(KK.GT.KF_NS(II,J1+J0)) GO TO 235
              IF(KK.LT.KG_NS(II,J1+J0)) VVBCN(II,KK,NN)=0.0D0
              IF(KK.LT.KG_NS(II,J1+J0)) GO TO 235
              IF(INDV_NS(II,J1,KK).GE.-1) THEN
                HZ1 = MIN(ZC_NS(1,KK),HH1)
                HZ2 = MAX(ZC_NS(1,KK-1),DEP)
C
                IF(K.EQ.KG1.AND.KK.EQ.KG_NS(II,J1+J0)) THEN
                  IF(HH_NS(II,J1+J0)-HDEP_NS(II,J1+J0).LE.GXB) THEN
                    IF(NN.EQ.1) THEN
                      IF(HH_NS(II,J1)-HDEP_ML(I,JJ).GT.GXB) THEN
                        IF(HH_NS(II,J1)-HDEP_NS(II,J1+1).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(II,J1)-HDEP_ML(I,JJ).LE.GXB.AND.
     $                   VVBCN(II,KK,NN).GE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    ELSE IF(NN.EQ.4) THEN
                      IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ+1).GT.GXB) THEN
                        IF(HH_NS(II,J1+1)-HDEP_NS(II,J1).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ+1).LE.GXB.AND.
     $                   VVBCN(II,KK,NN).LE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    END IF
                  ELSE
                    IF(NN.EQ.1) THEN
                      IF(HH_NS(II,J1)-HDEP_ML(I,JJ).LE.GXB) THEN
                        IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(II,J1)-HDEP_NS(II,J1+1).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    ELSE IF(NN.EQ.4) THEN
                      IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ+1).LE.GXB) THEN
                        IF(HH_NS(II,J1)-HDEP_ML(I,JJ+1).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(II,J1)-HDEP_ML(I,JJ+1).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(II,J1+1)-HDEP_NS(II,J1).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    END IF
                  END IF
                END IF
C
                SS  = XC_NS(4,II)*(HZ1-HZ2)
                IF(HZ1-HZ2.LT.GXB) THEN
                  SS = 0.0D0
                  VVBCN(II,KK,NN) = 0.0D0
                END IF
                IF(KK.NE.KG_NS(II,J1+J0)) SS=SS*GY_NS(II,J1,KK)
                SSUM = SSUM+SS
                VSUM = VSUM+SS*VVBCN(II,KK,NN)
              END IF
  235       CONTINUE
  225     CONTINUE
C
          HZ1 = HH_ML(I,JJ)*YC_ML(7,JJ)+HH_ML(I,JJ+1)*YC_ML(8,JJ)
          IF(IXX.EQ.0) THEN
            HZ2 = MAX(ZC_ML(1,K-1),DEP)
          ELSE
            KG2 = KG_NS(I1,J1+J0)
            HZK = MAX(ZC_NS(1,KG2-1),HDEP_NS(I1,J1+J0))
            HZ2 = MAX(ZC_ML(1,K-1),DEP,HZK)
          END IF
          IF(HZ1-HDEP_ML(I,JJ  ).LE.GXB.OR.
     $       HZ1-HDEP_ML(I,JJ+1).LE.GXB) HZ2=HZ1
          SS  = XC_ML(4,I)*(HZ1-HZ2)
          IF(K.NE.KG_ML(I,JJ+J0)) SS=SS*GY_ML(I,JJ,K)
          VDIF = SS*VV_ML(I,JJ,K)-VSUM
          IF(HZ1-HZ2.GE.GXB) THEN
            ISUM = 0
            IF(SSUM.EQ.0.0D0) THEN
              ISUM = 1
              VEPS= 0.0D0
            ELSE
              VEPS = VDIF/SSUM
            END IF
            DO 245 II=I1,I2
              HNS1 = HH_NS(II,J1)
              IF(HNS1.GT.1.0D10) HNS1=HDEP_NS(II,J1)
              HNS2 = HH_NS(II,J1+1)
              IF(HNS2.GT.1.0D10) HNS2=HDEP_NS(II,J1+1)
              HH1 = HNS1*YC_NS(7,J1)+HNS2*YC_NS(8,J1)
              DO 255 KK=K1,K2
                IF(KK.GT.KF_NS(II,J1+J0)) GO TO 255
                IF(KK.LT.KG_NS(II,J1+J0)) GO TO 255
                IF(INDV_NS(II,J1,KK).LT.-1) GO TO 255
                HZ1 = MIN(ZC_NS(1,KK),HH1)
                HZ2 = MAX(ZC_NS(1,KK-1),DEP)
C
                IF(K.EQ.KG1.AND.KK.EQ.KG_NS(II,J1+J0)) THEN
                  IF(HH_NS(II,J1+J0)-HDEP_NS(II,J1+J0).LE.GXB) THEN
                    IF(NN.EQ.1) THEN
                      IF(HH_NS(II,J1)-HDEP_ML(I,JJ).GT.GXB) THEN
                        IF(HH_NS(II,J1)-HDEP_NS(II,J1+1).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(II,J1)-HDEP_ML(I,JJ).LE.GXB.AND.
     $                   VVBCN(II,KK,NN).GE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    ELSE IF(NN.EQ.4) THEN
                      IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ+1).GT.GXB) THEN
                        IF(HH_NS(II,J1+1)-HDEP_NS(II,J1).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      END IF
                      IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ+1).LE.GXB.AND.
     $                   VVBCN(II,KK,NN).LE.0.0D0) THEN
                        HZ2 = HZ1
                      END IF
                    END IF
C
                    IF(ISUM.NE.0) THEN
                      WRITE(6,630) NN,ISUM,HNS1,HNS2,HH1,
     $                             HDEP_NS(II,J1),HDEP_NS(II,J1+1),DEP
  630                 FORMAT('NN,ISUM=',2I3,'  HHJ,HHP,HH1=',1P,3D12.5,
     $                                      '  DPJ,DPP,DEP=',3D12.5)
                    END IF
                  ELSE
                    IF(NN.EQ.1) THEN
                      IF(HH_NS(II,J1)-HDEP_ML(I,JJ).LE.GXB) THEN
                        IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).GE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(II,J1)-HDEP_NS(II,J1+1).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    ELSE IF(NN.EQ.4) THEN
                      IF(HH_NS(II,J1+1)-HDEP_ML(I,JJ+1).LE.GXB) THEN
                        IF(HH_NS(II,J1)-HDEP_ML(I,JJ+1).LE.GXB) THEN
                          HZ2 = HZ1
                        END IF
                        IF(VVBCN(II,KK,NN).LE.0.0D0) THEN
                          HZ2 = HZ1
                        END IF
                      ELSE
                        IF(HH_NS(II,J1)-HDEP_ML(I,JJ+1).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).GE.0.0D0) HZ2=HZ1
                        IF(HH_NS(II,J1+1)-HDEP_NS(II,J1).LE.GXB.AND.
     $                     VVBCN(II,KK,NN).LE.0.0D0) HZ2=HZ1
                      END IF
                    END IF
C
                    IF(ISUM.NE.0) THEN
                      ISUM = 2
                      WRITE(6,630) NN,ISUM,HNS1,HNS2,HH1,
     $                             HDEP_NS(II,J1),HDEP_NS(II,J1+1),DEP
                    END IF
                  END IF
                END IF
C
                IF(HZ1-HZ2.GE.GXB) THEN
                  VVBCN(II,KK,NN) = VVBCN(II,KK,NN)+VEPS
                  IF(ABS(VVBCN(II,KK,NN)).GT.VVMAX) THEN
                    WRITE(LP,600) ISTEP,TIME,I,JJ,K,VV_ML(I,JJ,K),
     $                            II,KK,NN,VVBCN(II,KK,NN)
 600                FORMAT('ISTEP,TIME=',I7,1P,D12.5,'  PARENT(I,J,K)=',
     $                     3I5,'  VV=',1P,D12.5,'  CHILD(II,KK,NN)=',
     $                     3I5,'  VVBCN=',1PD12.5)
                    VVBCN(II,KK,NN) = SIGN(VVMAX,VVBCN(II,KK,NN))
                  END IF
                  END IF
  255         CONTINUE
  245       CONTINUE
          ELSE
            DO 265 II=I1,I2
              DO 275 KK=K1,K2
                VVBCN(II,KK,NN) = 0.0D0
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
