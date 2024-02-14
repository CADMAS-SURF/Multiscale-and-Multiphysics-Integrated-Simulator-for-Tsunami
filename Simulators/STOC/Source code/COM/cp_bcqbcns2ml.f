      SUBROUTINE CP_BCQBCNS2ML(IEAS,IWES,JSOU,JNOR,
     $                      MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                      I_ML,J_ML,I_NS,J_NS,KF_ML,KF_NS,
     $                      XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
C----------------------------------------------------------------------
C     セル中心掃流砂量をMLの境界エリアに直接セットする
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CONNEC.h'
C
      INTEGER,INTENT(IN)::IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(IN)::I_ML(2,MX_ML),J_ML(2,MY_ML),
     $                    I_NS(2,MX_NS),J_NS(2,MY_NS)
      INTEGER,INTENT(IN)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KF_NS(MX_NS,MY_NS)
      REAL(8),INTENT(IN)::XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(IN)::QBXC_NS(MX_NS,MY_NS),QBYC_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::QBXC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                       QBYC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C ... ローカル変数
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
C ..... ML領域の境界エリアにセル中心の掃流砂量をセットする
        CALL CP_BCQBCML(IEAS,IWES,JSOU,JNOR,
     $                  IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP,
     $                  MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                  I_ML,J_ML,KF_ML,KF_NS,
     $                  XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
C
      RETURN
      END
