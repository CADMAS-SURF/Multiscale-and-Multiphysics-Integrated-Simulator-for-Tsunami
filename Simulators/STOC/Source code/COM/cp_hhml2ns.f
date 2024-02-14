cc
cc      CALL CP_HHML2NS(HH,HDEP,HHBCN,HHW,KF,XC,YC,I_ML,J_ML,
cc     $                MX_ML,MY_ML,IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,IFL)
cc
      SUBROUTINE CP_HHML2NS(HH,HDEP,HHBCN,HHW,KF,XC,YC,I_ML,J_ML,
     $                      MX_ML,MY_ML,IWES,IEAS,JSOU,JNOR,IFL)
C======================================================================
C     水位を重み付けする
C     HH: 水位(m)
C     HHBCN: 親領域での補間水位
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
C
      REAL(8),INTENT(INOUT)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::HHBCN(MX,MY),HHW(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX),YC(8,MY)
      INTEGER,INTENT(INOUT)::KF(MX,MY),I_ML(2,MX_ML),J_ML(2,MY_ML)
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,IWES,IEAS,JSOU,JNOR,IFL
C
      INTEGER::IDB=0
C
      REAL(8)::DH(MX,MY),DHSUM,SSSUM,EPSUM
      REAL(8)::HWD,SS,X1,X2,XEL,XER,XWL,XWR,Y1,Y2,YNB,YNT,YSB,YST
      INTEGER::I,J,IPARNT,II,JJ,I1,I2,J1,J2,NESYM,NESXM,NESXP,NESYP
C
      IF(IDB.NE.0.AND.IFL.EQ.0)  THEN
        WRITE(6,602) NESNS,IFL
 602    FORMAT('HHML2NS NESNS,IFL           =',5I5)
      END IF
C
      IPARNT = IPECON(2,NRANK+1)
      IF(IPARNT.GE.0) THEN
C
C ... 重み定数を設定する（最初のみ）
      IF(IFL.EQ.0) THEN
C
        XWL = XC(1,1)
        XWR = XC(1,1+NESNS(2))
        XER = XC(1,MXM)
        XEL = XC(1,MXM-NESNS(3))
        YSB = YC(1,1)
        YST = YC(1,1+NESNS(1))
        YNT = YC(1,MYM)
        YNB = YC(1,MYM-NESNS(4))
C
        IF(IDB.NE.0) THEN
          WRITE(6,*) 'XWL,XWR=',XWL,XWR
          WRITE(6,*) 'XEL,XER=',XEL,XER
          WRITE(6,*) 'YSB,YST=',YSB,YST
          WRITE(6,*) 'YNB,YNT=',YNB,YNT
        END IF
C
        IF(MY.EQ.3) THEN
          J = 2
          DO 100 I=2,MXM
            IF(KF(I,J).NE.MZ) THEN
              IF(XC(2,I).LT.XWR) THEN
                HHW(I,J) = 1.0D0-(XC(2,I)-XWL)/(XWR-XWL)
              ELSE IF(XC(2,I).GT.XEL) THEN
                HHW(I,J) = 1.0D0-(XER-XC(2,I))/(XER-XEL)
              ELSE
                HHW(I,J) = 0.0D0
              END IF
            END IF
  100     CONTINUE
C
        ELSE IF(MX.EQ.3) THEN
          I = 2
          DO 110 J=2,MYM
            IF(KF(I,J).NE.MZ) THEN
              IF(YC(2,J).LT.YST) THEN
                HHW(I,J) = 1.0D0-(YC(2,J)-YSB)/(YST-YSB)
              ELSE IF(YC(2,J).GT.YNB) THEN
                HHW(I,J) = 1.0D0-(YNT-YC(2,J))/(YNT-YNB)
              ELSE
                HHW(I,J) = 0.0D0
              END IF
            END IF
  110     CONTINUE
C
        ELSE
          DO 120 J=2+NESNS(1),MYM-NESNS(4)
          DO 125 I=2,MXM
            IF(KF(I,J).NE.MZ) THEN
              IF(XC(2,I).LT.XWR) THEN
                HHW(I,J) = 1.0D0-(XC(2,I)-XWL)/(XWR-XWL)
              ELSE IF(XC(2,I).GT.XEL) THEN
                HHW(I,J) = 1.0D0-(XER-XC(2,I))/(XER-XEL)
              ELSE
                HHW(I,J) = 0.0D0
              END IF
            END IF
  125     CONTINUE
  120     CONTINUE
C
          DO 130 I=2+NESNS(2),MXM-NESNS(3)
          DO 135 J=2,MYM
            IF(KF(I,J).NE.MZ) THEN
              IF(YC(2,J).LT.YST) THEN
                HHW(I,J) = 1.0D0-(YC(2,J)-YSB)/(YST-YSB)
              ELSE IF(YC(2,J).GT.YNB) THEN
                HHW(I,J) = 1.0D0-(YNT-YC(2,J))/(YNT-YNB)
              ELSE
                HHW(I,J) = 0.0D0
              END IF
            END IF
  135     CONTINUE
  130     CONTINUE
C
          SS = (XWR-XWL)*(YST-YSB)
          IF(SS.GT.0.0) THEN
            DO 140 J=2,2+NESNS(1)-1
            DO 145 I=2,2+NESNS(2)-1
              IF(KF(I,J).NE.MZ) THEN
                X1 = XC(2,I)-XWL
                X2 = XWR-XC(2,I)
                Y1 = YC(2,J)-YSB
                Y2 = YST-YC(2,J)
                HHW(I,J) = (X2*Y1+X1*Y2+X2*Y2)/SS
              END IF
  145       CONTINUE
  140       CONTINUE
          END IF
C
          SS = (XER-XEL)*(YST-YSB)
          IF(SS.GT.0.0) THEN
            DO 150 J=2,2+NESNS(1)-1
            DO 155 I=MXM-NESNS(3)+1,MXM
              IF(KF(I,J).NE.MZ) THEN
                X1 = XC(2,I)-XEL
                X2 = XER-XC(2,I)
                Y1 = YC(2,J)-YSB
                Y2 = YST-YC(2,J)
                HHW(I,J) = (X1*Y1+X1*Y2+X2*Y2)/SS
              END IF
  155       CONTINUE
  150       CONTINUE
          END IF
C
          SS = (XWR-XWL)*(YNT-YNB)
          IF(SS.GT.0.0) THEN
            DO 160 J=MYM-NESNS(4)+1,MYM
            DO 165 I=2,2+NESNS(2)-1
              IF(KF(I,J).NE.MZ) THEN
                X1 = XC(2,I)-XWL
                X2 = XWR-XC(2,I)
                Y1 = YC(2,J)-YNB
                Y2 = YNT-YC(2,J)
                HHW(I,J) = (X1*Y1+X2*Y1+X2*Y2)/SS
              END IF
  165       CONTINUE
  160       CONTINUE
          END IF
C
          SS = (XER-XEL)*(YNT-YNB)
          IF(SS.GT.0.0) THEN
            DO 170 J=MYM-NESNS(4)+1,MYM
            DO 175 I=MXM-NESNS(3)+1,MXM
              IF(KF(I,J).NE.MZ) THEN
                X1 = XC(2,I)-XEL
                X2 = XER-XC(2,I)
                Y1 = YC(2,J)-YNB
                Y2 = YNT-YC(2,J)
                HHW(I,J) = (X1*Y1+X2*Y1+X1*Y2)/SS
              END IF
  175       CONTINUE
  170       CONTINUE
          END IF
        END IF
C
        IF(IDB.NE.0) THEN
          WRITE(6,*) 'CP_HHML2NS HHW(WEIGHT VALUE)'
          CALL DBWR2D(HHW,3,1,MX,MY,1,6)
        END IF
C
      ELSE
C
C----------------------------------------------------------------------
C     補間水位と計算水位を重み平均する
C----------------------------------------------------------------------
        CALL ZERCLR(DH,MXY,0.0D0)
C
        DO 200 J=2,MYM
        DO 200 I=2,MXM
C ....... 水面のある領域のみ計算
          IF( KF(I,J).LT.MZ) THEN
            IF(HH(I,J)-HDEP(I,J).GE.GXB ) THEN
              HWD = HHW(I,J)
              IF(HHBCN(I,J).GT.1.0D10) HWD=0.0D0
              DH(I,J) = HH(I,J)*(1.0D0-HWD)+HHBCN(I,J)*HWD-HH(I,J)
c              HH(I,J) = HH(I,J)+DH(I,J)
c              HHBCN(I,J) = HH(I,J)
            ELSE IF(HH(I,J)-HDEP(I,J).LT.EPSH) THEN
              HH(I,J) = HDEP(I,J)+EPSH
              HHBCN(I,J) = HH(I,J)
            END IF
          END IF
  200   CONTINUE
C
C ..... 重み付け水位の補正
C
        NESXM = NOVRLP(1)
        NESYM = NOVRLP(2)
        NESXP = NOVRLP(3)
        NESYP = NOVRLP(4)
        DO 300 J=JSOU,JNOR
          J1 = J_ML(1,J-1)+1
          J2 = J_ML(1,J)
          DO 305 I=IWES,IWES+NESXM-1
            I1 = I_ML(2,I-1)+1
            I2 = I_ML(2,I)
            SSSUM = 0.0D0
            DHSUM = 0.0D0
            DO 310 JJ=J1,J2
            DO 310 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                SS = XC(4,II)*YC(4,JJ)
                SSSUM = SSSUM+SS
                DHSUM = DHSUM+SS*DH(II,JJ)
              END IF
  310       CONTINUE
c
            EPSUM = 0.0D0
            IF(SSSUM.NE.0.0D0) EPSUM=DHSUM/SSSUM
            DO 315 JJ=J1,J2
            DO 315 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                HH(II,JJ) = HH(II,JJ)+DH(II,JJ)-EPSUM
                IF(HH(II,JJ)-HDEP(II,JJ).LT.EPSH)
     $             HH(II,JJ) = HDEP(II,JJ)+EPSH
                HHBCN(II,JJ) = HH(II,JJ)
              END IF
  315       CONTINUE
  305     CONTINUE
  300   CONTINUE
C
        DO 320 J=JSOU,JNOR
          J1 = J_ML(1,J-1)+1
          J2 = J_ML(1,J)
          DO 325 I=IEAS-NESXP+1,IEAS
            I1 = I_ML(2,I-1)+1
            I2 = I_ML(2,I)
c            write(16,*) 'b:i1,i2=',i1,i2
c            write(16,*) 'b:j1,j2=',j1,j2
            SSSUM = 0.0D0
            DHSUM = 0.0D0
            DO 330 JJ=J1,J2
            DO 330 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                SS = XC(4,II)*YC(4,JJ)
                SSSUM = SSSUM+SS
                DHSUM = DHSUM+SS*DH(II,JJ)
              END IF
  330       CONTINUE
c
            EPSUM = 0.0D0
            IF(SSSUM.NE.0.0D0) EPSUM=DHSUM/SSSUM
            DO 335 JJ=J1,J2
            DO 335 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                HH(II,JJ) = HH(II,JJ)+DH(II,JJ)-EPSUM
                IF(HH(II,JJ)-HDEP(II,JJ).LT.EPSH)
     $             HH(II,JJ) = HDEP(II,JJ)+EPSH
                HHBCN(II,JJ) = HH(II,JJ)
              END IF
  335       CONTINUE
  325     CONTINUE
  320   CONTINUE
C
        DO 340 J=JSOU,JSOU+NESYM-1
          J1 = J_ML(1,J-1)+1
          J2 = J_ML(1,J)
          DO 345 I=IWES+NESXM,IEAS-NESXP
            I1 = I_ML(2,I-1)+1
            I2 = I_ML(2,I)
            SSSUM = 0.0D0
            DHSUM = 0.0D0
            DO 350 JJ=J1,J2
            DO 350 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                SS = XC(4,II)*YC(4,JJ)
                SSSUM = SSSUM+SS
                DHSUM = DHSUM+SS*DH(II,JJ)
              END IF
  350       CONTINUE
c
            EPSUM = 0.0D0
            IF(SSSUM.NE.0.0D0) EPSUM=DHSUM/SSSUM
            DO 355 JJ=J1,J2
            DO 355 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                HH(II,JJ) = HH(II,JJ)+DH(II,JJ)-EPSUM
                IF(HH(II,JJ)-HDEP(II,JJ).LT.EPSH)
     $             HH(II,JJ) = HDEP(II,JJ)+EPSH
                HHBCN(II,JJ) = HH(II,JJ)
              END IF
  355       CONTINUE
  345     CONTINUE
  340   CONTINUE
C
        DO 360 J=JNOR-NESYP+1,JNOR
          J1 = J_ML(1,J-1)+1
          J2 = J_ML(1,J)
          DO 365 I=IWES+NESXM,IEAS-NESXP
            I1 = I_ML(2,I-1)+1
            I2 = I_ML(2,I)
            SSSUM = 0.0D0
            DHSUM = 0.0D0
            DO 370 JJ=J1,J2
            DO 370 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                SS = XC(4,II)*YC(4,JJ)
                SSSUM = SSSUM+SS
                DHSUM = DHSUM+SS*DH(II,JJ)
              END IF
  370       CONTINUE
c
            EPSUM = 0.0D0
            IF(SSSUM.NE.0.0D0) EPSUM=DHSUM/SSSUM
            DO 375 JJ=J1,J2
            DO 375 II=I1,I2
              IF(KF(II,JJ).LT.MZ.AND.HH(II,JJ)-HDEP(II,JJ).GE.GXB) THEN
                HH(II,JJ) = HH(II,JJ)+DH(II,JJ)-EPSUM
                IF(HH(II,JJ)-HDEP(II,JJ).LT.EPSH)
     $             HH(II,JJ) = HDEP(II,JJ)+EPSH
                HHBCN(II,JJ) = HH(II,JJ)
              END IF
  375       CONTINUE
  365     CONTINUE
  360   CONTINUE
C
      END IF
C
      END IF
C
      RETURN
      END
