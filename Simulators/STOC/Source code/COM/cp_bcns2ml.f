      SUBROUTINE CP_BCNS2ML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                      INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                      XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                      GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                      I_ML,J_ML,K_ML,KF_ML,KG_ML,
     $                      I_NS,J_NS,KF_NS,KG_NS,
     5                      UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                      UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                      CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                      X1_NS,X2_NS,X1_ML,X2_ML,
     7                      MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     8                      IEAS,IWES,JSOU,JNOR,KBOT,KTOP,IFLAG)
C-----------------------------------------------------------------------
C     境界流速をMLのエリアに直接セットする
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'CONNEC.h'
CCCCCC
      INCLUDE 'BOUNDI.h'
CCCCCC
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
     $   INDW_NS(MX_NS,MY_NS,MZ_NS),INDP_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML),
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GY_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GZ_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GX_NS(MX_NS,MY_NS,MZ_NS),GY_NS(MX_NS,MY_NS,MZ_NS),
     $   GZ_NS(MX_NS,MY_NS,MZ_NS),GV_NS(MX_NS,MY_NS,MZ_NS)
C
      INTEGER,INTENT(INOUT)::
     $   I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML),
     $   I_NS(2,MX_NS),J_NS(2,MY_NS)
      INTEGER,INTENT(INOUT)::
     $   KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   UU_NS(MX_NS,MY_NS,MZ_NS),VV_NS(MX_NS,MY_NS,MZ_NS),
     $   WW_NS(MX_NS,MY_NS,MZ_NS),
     $   TT_NS(MX_NS,MY_NS,MZ_NS),CC_NS(MX_NS,MY_NS,MZ_NS),
     $   CSD_NS(MX_NS,MY_NS,MZ_NS),ZBD_NS(MX_NS,MY_NS),
     $   X1_NS(MX_NS,MY_NS,MZ_NS),X2_NS(MX_NS,MY_NS,MZ_NS),
     $   HH_NS(MX_NS,MY_NS),HDEP_NS(MX_NS,MY_NS)
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
C
      INTEGER::IDB=0
C
      INTEGER::IE,IS,JE,JS,NESTXM,NESTXP,NESTYM,NESTYP
C
      IS = I_NS(2,      2)
      IE = I_NS(2,MX_NS-1)
      JS = J_NS(2,      2)
      JE = J_NS(2,MY_NS-1)
      NESTYM = NOVRLP(1)
      NESTXM = NOVRLP(2)
      NESTXP = NOVRLP(3)
      NESTYP = NOVRLP(4)
      IF(IPECON(4,NRANK+1).GE.0) NESTYM=0
      IF(IPECON(5,NRANK+1).GE.0) NESTXM=0
      IF(IPECON(6,NRANK+1).GE.0) NESTXP=0
      IF(IPECON(7,NRANK+1).GE.0) NESTYP=0
C
C
CCCCC      IF(IFLAG.EQ.1) THEN
      IF(IFLAG.EQ.1.OR.IFLAG.EQ.2) THEN
C
C ..... ML領域の境界面における流速をセットする
C
      if(idb.ne.0) write(6,*) '# CP_BCVVML/S'
        CALL CP_BCVVML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                 INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                 XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                 GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                 I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                 UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                 UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                 CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                 X1_NS,X2_NS,X1_ML,X2_ML,
     7                 IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP,
     8                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
      if(idb.ne.0) write(6,*) '. CP_BCVVML/E'
C
CCCCC      ELSE
      END IF
      IF(IFLAG.EQ.2) THEN
C
C ..... ML領域の境界面における潮位をセットする
C
        CALL CP_BCHHML(XC_NS,YC_NS,I_ML,J_ML,KF_ML,KF_NS,HH_NS,
     1                 HH_ML,HDEP_NS,HDEP_ML,
     2                 IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP,
     3                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     4                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
        CALL CP_COMHHML(HH_ML,I_NS,J_NS,MX_ML,MY_ML,MX_NS,MY_NS,
     $                  IEAS,IWES,JSOU,JNOR)
      if(idb.ne.0) write(6,*) '# CP_BCHHML/E'
C
      END IF
C
      RETURN
      END
