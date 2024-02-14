      SUBROUTINE CP_BCQBCJ(JNS,NN,IEAS,IWES,JSOU,JNOR,
     $                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY_NS,
     $                     J_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                     KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
     $                     QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
C----------------------------------------------------------------------
C     セル中心の掃流砂量を子側で使用するために補間する(東西境界用)
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
C
      INTEGER,INTENT(IN)::JNS,NN,IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,MXY_NS
      INTEGER,INTENT(IN)::J_ML(2,MY_ML),I_NS(2,MX_NS),J_NS(2,MY_NS)
      INTEGER,INTENT(IN)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
      REAL(8),INTENT(IN)::XC_ML(8,MX_ML),YC_ML(8,MY_ML),
     $                    XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(IN)::ZBED_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    QBXC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    QBYC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(OUT)::ZBEDBCN(MXY_NS,4),
     $                     QBXCBCN(MXY_NS,4),QBYCBCN(MXY_NS,4)
C
      REAL(8)::X1,X2,Y1,Y2,SS,SSUM,XSUM,YSUM,BSUM
      INTEGER::IER,IML,IML1,IML2,JML,JML1,JML2,I
C
      JML = J_NS(1,JNS)
      IF(YC_NS(2,JNS).GE.YC_ML(2,JML))THEN
         JML1 = JML-1
         JML2 = JML
      ELSE
         JML1 = JML
         JML2 = JML+1
      ENDIF
      Y1=YC_NS(2,JNS)-YC_ML(2,JML1)
      Y2=YC_ML(2,JML2)-YC_NS(2,JNS)
C
      DO 100 I=2,MX_NS-1
C      IF(KF_NS(I,JNS).EQ.MZ_NS) GOTO 100
C
      IML = I_NS(1,I)
      IF(XC_NS(2,I).GE.XC_ML(2,IML)) THEN
        IML1 = IML
        IML2 = IML+1
      ELSE
        IML1 = IML-1
        IML2 = IML
      END IF
      X1=XC_NS(2,I)-XC_ML(2,IML1)
      X2=XC_ML(2,IML2)-XC_NS(2,I)
C
      SSUM = 0.0D0
      XSUM = 0.0D0
      YSUM = 0.0D0
      BSUM = 0.0D0
      IF(KF_ML(IML1,JML1).NE.MZ_ML)THEN
        SS = X2*Y2
        SSUM = SSUM+SS
        XSUM = XSUM+SS*QBXC_ML(IML1,JML1)
        YSUM = YSUM+SS*QBYC_ML(IML1,JML1)
        BSUM = BSUM+SS*ZBED_ML(IML1,JML1)
      END IF
      IF(KF_ML(IML2,JML1).NE.MZ_ML)THEN
        SS = X1*Y2
        SSUM = SSUM+SS
        XSUM = XSUM+SS*QBXC_ML(IML2,JML1)
        YSUM = YSUM+SS*QBYC_ML(IML2,JML1)
        BSUM = BSUM+SS*ZBED_ML(IML2,JML1)
      END IF
      IF(KF_ML(IML1,JML2).NE.MZ_ML)THEN
        SS = X2*Y1
        SSUM = SSUM+SS
        XSUM = XSUM+SS*QBXC_ML(IML1,JML2)
        YSUM = YSUM+SS*QBYC_ML(IML1,JML2)
        BSUM = BSUM+SS*ZBED_ML(IML1,JML2)
      END IF
      IF(KF_ML(IML2,JML2).NE.MZ_ML)THEN
        SS = X1*Y1
        SSUM = SSUM+SS
        XSUM = XSUM+SS*QBXC_ML(IML2,JML2)
        YSUM = YSUM+SS*QBYC_ML(IML2,JML2)
        BSUM = BSUM+SS*ZBED_ML(IML2,JML2)
      END IF
C
      IF(SSUM.GT.0.0) THEN
        QBXCBCN(I,NN) = XSUM/SSUM
        QBYCBCN(I,NN) = YSUM/SSUM
        ZBEDBCN(I,NN) = BSUM/SSUM
      ELSE
        QBXCBCN(I,NN) = 0.0D0
        QBYCBCN(I,NN) = 0.0D0
        ZBEDBCN(I,NN) = 0.0D0
      END IF
C
  100 CONTINUE
C
      RETURN
      END
