      SUBROUTINE CP_BCHHNS(XC_ML,YC_ML,XC_NS,YC_NS,I_NS,J_NS,
     1                     KF_ML,HH_ML,HDEP_ML,HHBCN,
     2                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,
     3                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C-----------------------------------------------------------------------
C     MLの境界値(潮位)をNS用のエリアに補間する
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'CP_NESTBC.h'
C
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'CONNEC.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
C
      REAL(8),INTENT(INOUT)::XC_ML(8,MX_ML),YC_ML(8,MY_ML)
      REAL(8),INTENT(INOUT)::XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      INTEGER,INTENT(INOUT)::I_NS(2,MX_NS),J_NS(2,MY_NS)
      INTEGER,INTENT(INOUT)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::HHBCN(MX_NS,MY_NS)
C
      INTEGER::IDB=0
      REAL(8)::HH1,S11,S12,S21,S22,SS1,X1,X2,XI,XM,XP,Y1,Y2,YJ,YM,YP
      INTEGER::I,I1,I2,IE,IES,II,IM,IP,IS,ISE,ISKIP
      INTEGER::J,J1,J2,JE,JES,JJ,JM,JP,JS,JSE,N1,N2
C
      IF(IDB.NE.0) WRITE(6,601) IWES,IEAS,JSOU,JNOR,KBOT,KTOP
 601  FORMAT('CP_BCHHNS IWES,IEAS,JSOU,JNOR,KBOT,KTOP=',6I5)
C
      CALL ZERCLR(HHBCN,MX_NS*MY_NS,0.0D0)
C
      JS = 1
      JE = MY_NS
      IF(MY_NS.EQ.3) THEN
        JS = 2
        JE = 2
      END IF
      IS = 1
      IE = MX_NS
      IF(MX_NS.EQ.3) THEN
        IS = 2
        IE = 2
      END IF
      if(idb.ne.0) write(6,*) 'cp_bchhns is,ie,js,je=',is,ie,js,je
C
      JSE = JS+NESNS(1)+1
      IF(NESNS(1).EQ.0) JSE=JS+1
      IF(IPECON(4,NRANK+1).GE.0) JSE=JS-1
      JES = JE-NESNS(4)-1
      IF(NESNS(4).EQ.0) JES=JE-1
      IF(IPECON(7,NRANK+1).GE.0) JES=JE+1
      ISE = IS+NESNS(2)+1
      IF(NESNS(2).EQ.0) ISE=IS+1
      IF(IPECON(5,NRANK+1).GE.0) ISE=IS-1
      IES = IE-NESNS(3)-1
      IF(NESNS(3).EQ.0) IES=IE-1
      IF(IPECON(6,NRANK+1).GE.0) IES=IE+1
      if(idb.ne.0) write(6,*) '  (ise,ies,jse,jes)  =',ise,ies,jse,jes
C
      DO 100 J = JS,JE
C
        ISKIP = 0
        IF(J.GT.JSE.AND.J.LT.JES) ISKIP=1
        JJ = J_NS(2,J)
        JM = JJ-1
        IF(JM.LT.JSOU-1) JM=JSOU-1
        JP = JJ+1
        IF(JP.GT.JNOR+1) JP=JNOR+1
        YJ = YC_ML(2,JJ)
        YM = YC_ML(2,JM)
        YP = YC_ML(2,JP)
C
CCC        IF(JJ.EQ.1.OR.(JJ.NE.MY_ML.AND.YC_NS(2,J).GE.YJ)) THEN
        IF(JJ.EQ.JSOU-1.OR.(JJ.NE.JNOR+1.AND.YC_NS(2,J).GE.YJ)) THEN
          Y1 = (YC_NS(2,J)-YJ)/(YP-YJ)
          Y2 = 1.0D0-Y1
          J1 = JP
          J2 = JJ
CCC        ELSE IF(JJ.EQ.MY_ML.OR.YC_NS(2,J).LT.YJ) THEN
        ELSE IF(JJ.EQ.JNOR+1.OR.YC_NS(2,J).LT.YJ) THEN
          Y1 = (YC_NS(2,J)-YM)/(YJ-YM)
          Y2 = 1.0D0-Y1
          J1 = JJ
          J2 = JM
        END IF
C
        N1 = J1
        N2 = J2
C
        DO 110 I = IS,IE
C
          IF(ISKIP.EQ.1.AND.(I.GT.ISE.AND.I.LT.IES)) GO TO 110
C
          J1 = N1
          J2 = N2
C
          II = I_NS(2,I)
          IM = II-1
          IF(IM.LT.1) IM=1
          IP = II+1
          IF(IP.GT.MX_ML) IP=MX_ML
          XI = XC_ML(2,II)
          XM = XC_ML(2,IM)
          XP = XC_ML(2,IP)
C
CCC          IF(II.EQ.1.OR.(II.NE.MX_NS.AND.XC_NS(2,I).GE.XI)) THEN
          IF(II.EQ.IWES-1.OR.(II.NE.IEAS+1.AND.XC_NS(2,I).GE.XI)) THEN
            X1 = (XC_NS(2,I)-XI)/(XP-XI)
            X2 = 1.0D0-X1
            I1 = IP
            I2 = II
CCC          ELSE IF(II.EQ.MX_NS.OR.XC_NS(2,I).LT.XI) THEN
          ELSE IF(II.EQ.IEAS+1.OR.XC_NS(2,I).LT.XI) THEN
            X1 = (XC_NS(2,I)-XM)/(XI-XM)
            X2 = 1.0D0-X1
            I1 = II
            I2 = IM
          END IF
C
          S11 = X1*Y1
          S12 = X1*Y2
          S21 = X2*Y1
          S22 = X2*Y2
          HH1 = 0.0D0
          SS1 = 0.0D0
          IF(KF_ML(I1,J1).NE.MZ_ML.AND.
     $       HH_ML(I1,J1)-HDEP_ML(I1,J1).GT.GXB) THEN
            SS1 = SS1+S11
            HH1 = HH1+S11*HH_ML(I1,J1)
          END IF
          IF(KF_ML(I2,J1).NE.MZ_ML.AND.
     $      HH_ML(I2,J1)-HDEP_ML(I2,J1).GT.GXB) THEN
            SS1 = SS1+S21
            HH1 = HH1+S21*HH_ML(I2,J1)
          END IF
          IF(KF_ML(I1,J2).NE.MZ_ML.AND.
     $      HH_ML(I1,J2)-HDEP_ML(I1,J2).GT.GXB) THEN
            SS1 = SS1+S12
            HH1 = HH1+S12*HH_ML(I1,J2)
          END IF
          IF(KF_ML(I2,J2).NE.MZ_ML.AND.
     $      HH_ML(I2,J2)-HDEP_ML(I2,J2).GT.GXB) THEN
            SS1 = SS1+S22
            HH1 = HH1+S22*HH_ML(I2,J2)
          END IF
C
          IF(SS1.GT.0.0D0) THEN
            HHBCN(I,J) = HH1/SS1
          ELSE
            HHBCN(I,J) = 1.0D20
          END IF
          if(idb.ne.0)
     a    write(6,11) i,j,i1,j1,i2,j2,ii,jj,hhbcn(i,j)
 11       format('i,j,i1,j1,i2,j2,ii,jj,hhbcn(i,j)=',8i5,1p,d22.15)
C
  110   CONTINUE
        if(idb.ne.0.and.j.eq.2)
     b    write(6,*) 'BCHHNS J,HHBCN (1,2,mxm,mx)=',J,
     a    hhbcn(1,2),hhbcn(2,2),hhbcn(mx_ns-1,2),hhbcn(mx_ns,2)
C
  100 CONTINUE
C
      RETURN
      END
