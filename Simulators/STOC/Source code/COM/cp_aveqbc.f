      SUBROUTINE CP_AVEQBC(IEAS,IWES,JSOU,JNOR,I1,I2,J1,J2,
     $                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                     I_ML,J_ML,KF_ML,KF_NS,
     $                     XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
C----------------------------------------------------------------------
C     NSのセル中心掃流砂量をMLのエリアに直接セットする
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER,INTENT(IN)::IEAS,IWES,JSOU,JNOR,I1,I2,J1,J2
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(IN)::I_ML(2,MX_ML),J_ML(2,MY_ML)
      INTEGER,INTENT(IN)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KF_NS(MX_NS,MY_NS)
      REAL(8),INTENT(IN)::XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(IN)::QBXC_NS(MX_NS,MY_NS),QBYC_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::QBXC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                       QBYC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C ... ローカル変数
      REAL(8)::SS,SSUM,XSUM,YSUM
      INTEGER::II,II1,II2,JJ,JJ1,JJ2
C
C ... 平均掃流砂量(QBXC,QBYC)を計算
      IF(KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        SSUM = 0.0D0
        XSUM = 0.0D0
        YSUM = 0.0D0
        DO 100 JJ=JJ1,JJ2
        DO 100 II=II1,II2
           IF(KF_NS(II,JJ).EQ.MZ_NS) CYCLE
           SS = XC_NS(4,II)*YC_NS(4,JJ)
           SSUM = SSUM+SS
           XSUM = XSUM+SS*QBXC_NS(II,JJ)
           YSUM = YSUM+SS*QBYC_NS(II,JJ)
  100   CONTINUE
C
        IF(SSUM.NE.0.0D0) THEN
           QBXC_ML(I1,J1) = XSUM/SSUM
           QBYC_ML(I1,J1) = YSUM/SSUM
        END IF
C
      END IF
C
      RETURN
      END
