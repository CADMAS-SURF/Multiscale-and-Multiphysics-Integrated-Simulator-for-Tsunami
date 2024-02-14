      SUBROUTINE CP_BCQBCML2NS(IEAS,IWES,JSOU,JNOR,
     $                      MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                      I_ML,J_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                      KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
     $                      QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
C----------------------------------------------------------------------
C     補間して仮想セル用のセル中心掃流砂量を求める
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE  'CP_NESTBC.h'
      INCLUDE  'TIMEI.h'
      INCLUDE  'FILE.h'
      INCLUDE  'CONNEC.h'
      INCLUDE  'DOMAIN.h'
C
      INTEGER,INTENT(IN)::IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(IN)::I_ML(2,MX_ML),J_ML(2,MY_ML),
     $                    I_NS(2,MX_NS),J_NS(2,MY_NS)
      INTEGER,INTENT(IN)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
      REAL(8),INTENT(IN)::XC_ML(8,MX_ML),YC_ML(8,MY_ML),
     $                    XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(IN)::ZBED_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    QBXC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    QBYC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::ZBEDBCN(MXY,4),
     $                       QBXCBCN(MXY,4),QBYCBCN(MXY,4)
C
      INTEGER::INS,JNS,NN
C

C ... J=1,(I=2,MXM_NS)の仮想セル
      IF(IPECON(4,NRANK+1).LT.0) THEN
        JNS= 1
        NN = 1
        CALL CP_BCQBCJ(JNS,NN,IEAS,IWES,JSOU,JNOR,
     $                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY,
     $                 I_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                 KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
     $                 QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
      END IF
C
C ... I=1,(J=2,MYM_NS)の仮想セル
      IF(IPECON(5,NRANK+1).LT.0) THEN
        INS= 1
        NN = 2
        CALL CP_BCQBCI(INS,NN,IEAS,IWES,JSOU,JNOR,
     $                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY,
     $                 J_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                 KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
     $                 QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
      END IF
C
C ... I=MX_NS-1,(J=2,MYM_NS)の仮想セル
      IF(IPECON(6,NRANK+1).LT.0) THEN
        INS= MX_NS
        NN = 3
        CALL CP_BCQBCI(INS,NN,IEAS,IWES,JSOU,JNOR,
     $                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY,
     $                 J_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                 KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
     $                 QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
      END IF
C
C ... J=MY_NS=1,(I=2,MXM_NS)の仮想セル
      IF(IPECON(7,NRANK+1).LT.0) THEN
        JNS= MY_NS
        NN = 4
        CALL CP_BCQBCJ(JNS,NN,IEAS,IWES,JSOU,JNOR,
     $                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY,
     $                 I_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                 KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
     $                 QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
      END IF
C
C
      RETURN
      END
