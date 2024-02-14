      SUBROUTINE CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                      INDU_NS,INDV_NS,INDP_NS,
     1                      XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                      GX_ML,GX_NS,GY_ML,GY_NS,
     3                      I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     4                      KF_ML,KG_ML,KF_NS,KG_NS,
     5                      UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,X1_ML,X2_ML,
     *                      HH_ML,HDEP_ML,HDEP_NS,CSD_ML,ZBD_ML,
     6                      HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,
     *                      X1BCN,X2BCN,CSDBCN,ZBDBCN,
     8                      MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                      IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     A                      IFLAG)
C-----------------------------------------------------------------------
C     MLの境界値(流速,潮位)を時間補間用のエリアにセットする
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE  'CP_NESTBC.h'
      INCLUDE  'TIMEI.h'
      INCLUDE  'FILE.h'
      INCLUDE  'CONNEC.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
      INTEGER,INTENT(INOUT)::IFLAG
C
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDU_NS(MX_NS,MY_NS,MZ_NS),INDV_NS(MX_NS,MY_NS,MZ_NS),
     $   INDP_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML),
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GY_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GX_NS(MX_NS,MY_NS,MZ_NS),GY_NS(MX_NS,MY_NS,MZ_NS)
C
      INTEGER,INTENT(INOUT)::
     $   I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML),
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
     $   ZBD_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   X1_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   X2_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::
     $   HDEP_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   HHBCN(MX_NS,MY_NS),UUBCN(NXY,MZ_NS,4),VVBCN(NXY,MZ_NS,4),
     $   WWBCN(NXY,MZ_NS,4),TTBCN(NXY,MZ_NS,4),CCBCN(NXY,MZ_NS,4),
     $   CSDBCN(NXY,MZ_NS,4),ZBDBCN(MX_NS,MY_NS),
     $   X1BCN(NXY,MZ_NS,4),X2BCN(NXY,MZ_NS,4)
C
      INTEGER::IDB=0
C
      INTEGER::I1,I2,J1,J2,NN
C
      IF(IDB.NE.0) WRITE(6,601) IWES,IEAS,JSOU,JNOR,KBOT,KTOP,IFLAG
 601  FORMAT('CP_BCML2NS IWES,IEAS,JSOU,JNOR,KBOT,KTOP,IFLAG=',7I5)
C
CCC      IF(IFLAG.EQ.0.OR.IFLAG.EQ.2) THEN
CCC      IF(IFLAG.EQ.1) THEN    ! 2005.01.19
      IF(IFLAG.NE.2) THEN
C
        CALL ZERCLR(UUBCN,NXY*MZ_NS*4,0.0D0)
        CALL ZERCLR(VVBCN,NXY*MZ_NS*4,0.0D0)
        CALL ZERCLR(WWBCN,NXY*MZ_NS*4,0.0D0)
C
C ... 境界面での流速、温度、濃度
C
C     J=1,(I=2,MXM_NS)面での流速、温度、濃度
C
        IF(IPECON(4,NRANK+1).LT.0) THEN
        J1 = 1
        J2 = 2
        NN = 1
        CALL CP_BCUVWJ(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                 INDV_NS,INDP_NS,
     1                 XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                 GY_ML,GY_NS,I_ML,K_ML,I_NS,J_NS,K_NS,
     3                 KF_ML,KG_ML,KF_NS,KG_NS,HDEP_ML,HDEP_NS,
     4                 UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HHBCN,
     $                 X1_ML,X2_ML,X1BCN,X2BCN,CSD_ML,CSDBCN,
     5                 UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,J1,J2,NN,
     6                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,NXY,
     7                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
        END IF
C
C     I=1,(J=2,MYM_NS)面での流速、温度、濃度
C
        IF(IPECON(5,NRANK+1).LT.0) THEN
        I1 = 1
        I2 = 2
        NN = 2
        CALL CP_BCUVWI(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                 INDU_NS,INDP_NS,
     1                 XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                 GX_ML,GX_NS,J_ML,K_ML,I_NS,J_NS,K_NS,
     3                 KF_ML,KG_ML,KF_NS,KG_NS,HDEP_ML,HDEP_NS,
     4                 UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HHBCN,
     $                 X1_ML,X2_ML,X1BCN,X2BCN,CSD_ML,CSDBCN,
     5                 UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,I1,I2,NN,
     6                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,NXY,
     7                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
        END IF
C
C     I=MX_NS-1,(J=2,MYM_NS)面での流速、温度、濃度
C
        IF(IPECON(6,NRANK+1).LT.0) THEN
        I1 = MX_NS-1
        I2 = MX_NS-1
        NN = 3
        CALL CP_BCUVWI(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                 INDU_NS,INDP_NS,
     1                 XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                 GX_ML,GX_NS,J_ML,K_ML,I_NS,J_NS,K_NS,
     3                 KF_ML,KG_ML,KF_NS,KG_NS,HDEP_ML,HDEP_NS,
     4                 UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HHBCN,
     $                 X1_ML,X2_ML,X1BCN,X2BCN,CSD_ML,CSDBCN,
     5                 UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,I1,I2,NN,
     6                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,NXY,
     7                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
        END IF
C
C     J=MY_NS=1,(I=2,MXM_NS)面での流速、温度、濃度
C
        IF(IPECON(7,NRANK+1).LT.0) THEN
        J1 = MY_NS-1
        J2 = MY_NS-1
        NN = 4
        CALL CP_BCUVWJ(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                 INDV_NS,INDP_NS,
     1                 XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     2                 GY_ML,GY_NS,I_ML,K_ML,I_NS,J_NS,K_NS,
     3                 KF_ML,KG_ML,KF_NS,KG_NS,HDEP_ML,HDEP_NS,
     4                 UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HHBCN,
     $                 X1_ML,X2_ML,X1BCN,X2BCN,CSD_ML,CSDBCN,
     5                 UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,J1,J2,NN,
     6                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,NXY,
     7                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
        END IF
C
CCC      ELSE         ! 2005.01.18
      END IF
      IF(IFLAG.NE.1) THEN
C
C ... NS領域の表面水位
C
        CALL CP_BCHHNS(XC_ML,YC_ML,XC_NS,YC_NS,I_NS,J_NS,
     1                 KF_ML,HH_ML,HDEP_ML,HHBCN,
     2                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,
     3                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C
      END IF
C
      RETURN
      END
