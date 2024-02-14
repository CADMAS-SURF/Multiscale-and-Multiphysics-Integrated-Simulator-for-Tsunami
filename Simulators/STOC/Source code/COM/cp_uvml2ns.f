      SUBROUTINE CP_UVML2NS(UU,VV,UP,VP,INDU,INDV,UUBCN,VVBCN,KF,
     $                      XC,YC,ZC)
C======================================================================
C     流速を重み付け平均する
C     UU,VV:子側の流速
C     UUBCN,VVBCN: 親側の流速を境界部に補間した流速
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CP_NESTBC.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UP(MX,MY,MZ),VP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
      INTEGER,INTENT(INOUT)::KF(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX),YC(8,MY),ZC(8,MZ)
C
      REAL(8)::AA=1.0D0,BB=0.0D0
C
      REAL(8)::DDX,DDY
      REAL(8)::DUII,DUIP,DUJJ,DUJP,DUKK,DUKP
      REAL(8)::DVII,DVIP,DVJJ,DVJP,DVKK,DVKP
      REAL(8)::UUBCI,VVBCJ,X1,X2,Y1,Y2
      INTEGER::I,IE,IE2,IS,IS2,J,JE,JE2,JS,JS2,K
      INTEGER::NESNS1,NESNS2,NESNS3,NESNS4
C
      IF(IPECON(2,NRANK+1).LT.0) RETURN
C
      DO 100 K=1,MZ
      DO 100 J=1,MY
      DO 100 I=1,MX
        UP(I,J,K) = UU(I,J,K)
        VP(I,J,K) = VV(I,J,K)
  100 CONTINUE
C
      DO 200 K=2,MZM
      DO 200 J=2,MYM
C
C ... WEST
        IF(NESNS(2).GT.1) THEN
          X1 = XC(1,1)
          X2 = XC(1,NESNS(2)+1)
          DDX = 1.0D0/(X2-X1)/AA
          IE = NESNS(2)+1
          DO 210 I=2,IE
            IF(INDU(1,J,K).EQ.-1) THEN
              IF(INDU(I,J,K).GT.0) THEN
                UU(I,J,K) = UP(I,J,K)
     $                + DDX*(X2-XC(1,I))*(UUBCN(J,K,2)-UP(I,J,K))
              END IF
            END IF
  210     CONTINUE
C
          IF(J.GT.NESNS(1).AND.J.LT.MY-NESNS(4)) THEN
            VVBCJ = YC(7,J)*VVBCN(J,K,2)+YC(8,J)*VVBCN(J+1,K,2)
            DO 220 I=2,IE
              IF(INDU(1,J,K).EQ.-1.AND.INDU(1,J+1,K).EQ.-1) THEN
                IF(INDV(I,J,K).GT.0) THEN
                  VV(I,J,K) = VP(I,J,K)
     $                      + DDX*(X2-XC(2,I))*(VVBCJ-VP(I,J,K))
                 END IF
              END IF
  220       CONTINUE
          END IF
C
          IF(BB.GT.0.0D0) THEN
            NESNS2 = 2*NESNS(2)
            X1 = XC(1,1)
            X2 = XC(1,NESNS2+1)
            DDX = 1.0D0/(X2-X1)/BB
            IE2 = NESNS2+1
            DO 230 I=2,IE2
              IF(K.GE.KF(I,J).OR.K.GE.KF(I+1,J)) GO TO 230
              IF(INDU(1,J,K).EQ.-1) THEN
                IF(INDU(I,J,K).GT.0) THEN
                  DUIP = UP(I+1,J,K)-UP(I,J,K)
                  DUII = UP(I,J,K)-UP(I-1,J,K)
                  DUJP = UP(I,J+1,K)-UP(I,J,K)
                  IF(INDU(I,J+1,K).LE.0) DUJP=0.0D0
                  DUJJ = UP(I,J,K)-UP(I,J-1,K)
                  IF(INDU(I,J-1,K).LE.0) DUJJ=0.0D0
                  DUKP = UP(I,J,K+1)-UP(I,J,K)
                  IF(K+1.GE.KF(I,J).OR.K+1.GE.KF(I+1,J)) DUKP=0.0D0
                  DUKK = UP(I,J,K)-UP(I,J,K-1)
                  IF(INDU(I,J,K-1).LE.0) DUKK=0.0D0
                  UU(I,J,K) = UU(I,J,K)
     $            +    DDX*(X2-XC(1,I))*(DUIP-DUII+DUJP-DUJJ+DUKP-DUKK)
                END IF
              END IF
  230       CONTINUE
          END IF
        END IF
C
C ... EAST
        IF(NESNS(3).GT.1) THEN
          X1 = XC(1,MXM-NESNS(3))
          X2 = XC(1,MXM)
          DDX = 1.0D0/(X2-X1)/AA
          IS = MXM-NESNS(3)
          DO 240 I=MXM-1,IS,-1
            IF(INDU(MXM,J,K).EQ.-1) THEN
              IF(INDU(I,J,K).GT.0) THEN
                UU(I,J,K) = UP(I,J,K)
     $                    + DDX*(XC(1,I)-X1)*(UUBCN(J,K,3)-UP(I,J,K))
              END IF
            END IF
  240     CONTINUE
C
          IF(J.GT.NESNS(1).AND.J.LT.MY-NESNS(4)) THEN
            VVBCJ = YC(7,J)*VVBCN(J,K,3)+YC(8,J)*VVBCN(J+1,K,3)
            DO 250 I=MXM,IS+1,-1
              IF(INDU(MXM,J,K).EQ.-1.AND.INDU(MXM,J,K).EQ.-1) THEN
                IF(INDV(I,J,K).GT.0) THEN
                  VV(I,J,K) = VP(I,J,K)
     $                      + DDX*(XC(2,I)-X1)*(VVBCJ-VP(I,J,K))
                END IF
              END IF
  250       CONTINUE
          END IF
C
          IF(BB.GT.0.0D0) THEN
            NESNS3 = 2*NESNS(3)
            X1 = XC(1,MXM-NESNS3)
            X2 = XC(1,MXM)
            DDX = 1.0D0/(X2-X1)/BB
            IS2 = MXM-NESNS3
            DO 260 I=MXM-1,IS2,-1
              IF(K.GE.KF(I,J).OR.K.GE.KF(I+1,J)) GO TO 260
              IF(INDU(MXM,J,K).EQ.-1) THEN
                IF(INDU(I,J,K).GT.0) THEN
                  DUIP = UP(I+1,J,K)-UP(I,J,K)
                  DUII = UP(I,J,K)-UP(I-1,J,K)
                  DUJP = UP(I,J+1,K)-UP(I,J,K)
                  IF(INDU(I,J+1,K).LE.0) DUJP=0.0D0
                  DUJJ = UP(I,J,K)-UP(I,J-1,K)
                  IF(INDU(I,J-1,K).LE.0) DUJJ=0.0D0
                  DUKP = UP(I,J,K+1)-UP(I,J,K)
                  IF(K+1.GE.KF(I,J).OR.K+1.GE.KF(I+1,J)) DUKP=0.0D0
                  DUKK = UP(I,J,K)-UP(I,J,K-1)
                  IF(INDU(I,J,K-1).LE.0) DUKK=0.0D0
                  UU(I,J,K) = UU(I,J,K)
     $            +    DDX*(XC(1,I)-X1)*(DUIP-DUII+DUJP-DUJJ+DUKP-DUKK)
                END IF
              END IF
  260       CONTINUE
          END IF
        END IF
C
  200 CONTINUE
C
      DO 300 K=2,MZM
      DO 300 I=2,MXM
C
C ... SOUTH
        IF(NESNS(1).GT.1) THEN
          Y1 = YC(1,1)
          Y2 = YC(1,NESNS(1)+1)
          DDY = 1.0D0/(Y2-Y1)/AA
          JE = NESNS(1)+1
          DO 310 J=2,JE
            IF(INDV(I,1,K).EQ.-1) THEN
              IF(INDV(I,J,K).GT.0) THEN
                VV(I,J,K) = VP(I,J,K)
     $                    + DDY*(Y2-YC(1,J))*(VVBCN(I,K,1)-VP(I,J,K))
              END IF
            END IF
  310     CONTINUE
C
          IF(I.GT.NESNS(2).AND.I.LT.MX-NESNS(3)) THEN
            UUBCI = XC(7,I)*UUBCN(I,K,1)+XC(8,I)*UUBCN(I+1,K,1)
            DO 320 J=2,JE
              IF(INDV(I,1,K).EQ.-1.AND.INDV(I+1,1,K).EQ.-1) THEN
                IF(INDU(I,J,K).GT.0) THEN
                  UU(I,J,K) = UP(I,J,K)
     $                      + DDY*(Y2-YC(2,J))*(UUBCI-UP(I,J,K))
                END IF
              END IF
  320       CONTINUE
          ENDIF
C
          IF(BB.GT.0.0D0) THEN
            NESNS1 = 2*NESNS(1)
            Y1 = YC(1,1)
            Y2 = YC(1,NESNS1+1)
            DDY = 1.0D0/(Y2-Y1)/BB
            JE2 = NESNS1+1
            DO 330 J=2,JE2
              IF(K.GE.KF(I,J).OR.K.GE.KF(I,J+1)) GO TO 330
              IF(INDV(I,1,K).EQ.-1) THEN
                IF(INDV(I,J,K).GT.0) THEN
                  DVJP = VP(I,J+1,K)-VP(I,J,K)
                  DVJJ = VP(I,J,K)-VP(I,J-1,K)
                  DVIP = VP(I+1,J,K)-VP(I,J,K)
                  IF(INDV(I+1,J,K).LE.0) DVIP=0.0D0
                  DVII = VP(I,J,K)-VP(I-1,J,K)
                  IF(INDV(I-1,J,K).LE.0) DVII=0.0D0
                  DVKP = VP(I,J,K+1)-VP(I,J,K)
                  IF(K+1.GE.KF(I,J).OR.K+1.GE.KF(I,J+1)) DVKP=0.0D0
                  DVKK = VP(I,J,K)-VP(I,J,K-1)
                  IF(INDV(I,J,K-1).LE.0) DVKK=0.0D0
                  VV(I,J,K) = VV(I,J,K)
     $            +    DDY*(Y2-YC(1,J))*(DVJP-DVJJ+DVIP-DVII+DVKP-DVKK)
                END IF
              END IF
  330       CONTINUE
          END IF
        END IF
C
C ... NORTH
        IF(NESNS(4).GT.1) THEN
          Y1 = YC(1,MYM-NESNS(4))
          Y2 = YC(1,MYM)
          DDY = 1.0D0/(Y2-Y1)/AA
          JS = MYM-NESNS(4)
          DO 340 J=MYM-1,JS,-1
            IF(INDV(I,MYM,K).EQ.-1) THEN
              IF(INDV(I,J,K).GT.0) THEN
                VV(I,J,K) = VP(I,J,K)
     $                    + DDY*(YC(1,J)-Y1)*(VVBCN(I,K,4)-VP(I,J,K))
              END IF
            END IF
  340     CONTINUE
C
          IF(I.GT.NESNS(2).AND.I.LT.MX-NESNS(3)) THEN
            UUBCI = XC(7,I)*UUBCN(I,K,4)+XC(8,I)*UUBCN(I+1,K,4)
            DO 350 J=MYM,JS+1,-1
              IF(INDV(I,MYM,K).EQ.-1.AND.INDV(I+1,MYM,K).EQ.-1) THEN
                IF(INDU(I,J,K).GT.0) THEN
                  UU(I,J,K) = UP(I,J,K)
     $                      + DDY*(YC(2,J)-Y1)*(UUBCI-UP(I,J,K))
                END IF
              END IF
  350       CONTINUE
          END IF
C
          IF(BB.GT.0.0D0) THEN
            NESNS4 = 2*NESNS(4)
            Y1 = YC(1,MYM-NESNS4)
            Y2 = YC(1,MYM)
            DDY = 1.0D0/(Y2-Y1)/BB
            JS2 = MYM-NESNS4
            DO 360 J=MYM-1,JS2,-1
              IF(K.GE.KF(I,J).OR.K.GE.KF(I,J+1)) GO TO 360
              IF(INDV(I,MYM,K).EQ.-1) THEN
                IF(INDV(I,J,K).GT.0) THEN
                  DVJP = VP(I,J+1,K)-VP(I,J,K)
                  DVJJ = VP(I,J,K)-VP(I,J-1,K)
                  DVIP = VP(I+1,J,K)-VP(I,J,K)
                  IF(INDV(I+1,J,K).LE.0) DVIP=0.0D0
                  DVII = VP(I,J,K)-VP(I-1,J,K)
                  IF(INDV(I-1,J,K).LE.0) DVII=0.0D0
                  DVKP = VP(I,J,K+1)-VP(I,J,K)
                  IF(K+1.GE.KF(I,J).OR.K+1.GE.KF(I,J+1)) DVKP=0.0D0
                  DVKK = VP(I,J,K)-VP(I,J,K-1)
                  IF(INDV(I,J,K-1).LE.0) DVKK=0.0D0
                  VV(I,J,K) = VV(I,J,K)
     $            +    DDY*(YC(1,J)-Y1)*(DVJP-DVJJ+DVIP-DVII+DVKP-DVKK)
                END IF
              END IF
  360       CONTINUE
          END IF
        END IF
C
  300 CONTINUE
C
      RETURN
      END
