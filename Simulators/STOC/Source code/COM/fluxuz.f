      SUBROUTINE FLUXUZ(FW,UU,VV,WW,HW,UUZ,FF,TMU,XC,ZC,GV,GZ,GV0,HH,
     $                  HDEP,DU,DIMP,AMNG,WX,WY,CD,INDU,INDW,KG,KP,KF)
C======================================================================
C     X方向の運動量保存式のZ方向界面の運動量流束を計算する
C     FW: X方向格子点、Y方向セル中心、Z方向格子点で定義
C
C     2つのセル面にまたがる界面の場合分け
C     (1) 境界条件なし
C     (2) 両方とも自由流入出境界
C     (3) 両方とも速度固定出境界
C     (4) 壁境界または2種類の境界条件が存在
C         海底摩擦項をマニングの粗度係数型に変更
C     (5) 表面摩擦係数を関数型に変更
C     ISW(3).NE.0 : 表層の流速を計算する
C     ISW(4).NE.0 : 慣性項をゼロ
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(INOUT)::FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HW(MX,MY,MZ),UUZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::DU(MX,MY,MZ),DIMP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AMNG(MX,MY),WX(MX,MY),WY(MX,MY),CD(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KG(MX,MY),KP(MX,MY),KF(MX,MY)
C
      REAL(8)::ADV1,ADV1M,ADV1P,VIS1,VIS1M,VIS1P
      REAL(8)::UU1,UU2,GZ1,HW1,TMU0,TMU1,TMU2,ZC1
      REAL(8)::DHH,DH1,DH2,BMNG,CDX,CEXP,CIMP,RES1,RES2,REXP
      REAL(8)::UUUU,VVUU,WXUU,WYUU,VABS,WABS,TAU1
      REAL(8)::HH0,HH1,HH2,HHB,COEF0,COEF1
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KP1,KF1,KNB1,INDW1,INDW2,IWIND
      INTEGER,PARAMETER::IMPSRF=0
C
C
      REXP = 1.0D0 - RIMP
      IWIND = 0
      IF(IGM2S.NE.0.OR.ISURF(1).LE.-1 ) IWIND=1
C
      CALL ZERCLR(FW,MXYZ,0.0D0)
C
      DO 100 K=1,MZM
      DO 100 J=2,MYM
C     DO 100 I=2,MXM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 I=2,MXM
C
         KP1 = MAX(KP(I,J),KP(I+1,J))
         KF1 = MIN(MAX(KF(I,J),KF(I+1,J)),MZM)
         IF(LSURF.EQ.0) KF1=MZM+1
         IF( K.LE.KF1-1 ) THEN
C
C ...... ±Zのどちらかの側が流速のX方向成分計算点である
         IF( INDU(I,J,K).GT.0 .OR. INDU(I,J,K+1).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDU(I,J,K).GT.0 ) THEN
               KNB1 = K
               ZC1  = 2.0D0*ZC(6,K)
            ELSE
               KNB1 = K+1
               ZC1  = -2.0D0*ZC(6,K+1)
            END IF
C
            INDW1 = INDW(I  ,J,K)
            INDW2 = INDW(I+1,J,K)
C
C ......... (1) 境界条件なし
            IF( INDW1.GT.0 .AND. INDW2.GT.0 ) THEN
C
C ............ 粘性項
               GZ1  = GZ(I,J,K)*XC(8,I,J)+GZ(I+1,J,K)*XC(7,I,J)
               TMU1 = TMU(I,J,K  )*XC(7,I,J)+TMU(I+1,J,K  )*XC(8,I,J)
               TMU2 = TMU(I,J,K+1)*XC(7,I,J)+TMU(I+1,J,K+1)*XC(8,I,J)
               TMU0 = TMU1*ZC(7,K)+TMU2*ZC(8,K)
               VIS1 = GZ1*(ANUV+TMU0)*((UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
     $              +                 (WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J))
C
C ............ 慣性項
               UU1 = UU(I,J,K)*ZC(7,K)+UU(I,J,K+1)*ZC(8,K)
               HW1 = HW(I,J,K)*XC(8,I,J)+HW(I+1,J,K)*XC(7,I,J)
               ADV1 = PARAMV2* UU1*HW1
     $              + PARAMV *(UU(I,J,K  )*MAX(HW1,0.0D0)
     $                        +UU(I,J,K+1)*MIN(HW1,0.0D0))
C
               IF( IMPSRF.EQ.1.AND.K.EQ.KF1-1 ) THEN
                  HH1 = MAX(FF(I  ,J,K+1)-1.0D0+GV0(I  ,J,K+1),0.0D0)
     $                *GV(I  ,J,K+1)/GV0(I  ,J,K+1)
                  HH2 = MAX(FF(I+1,J,K+1)-1.0D0+GV0(I+1,J,K+1),0.0D0)
     $                *GV(I+1,J,K+1)/GV0(I+1,J,K+1)
                  HH0 = HH1*XC(8,I,J)+HH2*XC(7,I,J)
                  COEF0 = GZ1*(ANUV+TMU0)*(1.0D0-HH0)*ZC(5,K)*ZC(6,K+1)
                  DU(I,J,K+1) = DU(I,J,K+1) + COEF0*UU(I,J,K+1)
                  DIMP(I,J,K+1) = DIMP(I,J,K+1) + COEF0
               END IF
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDW1.EQ.0 .AND. INDW2.EQ.0 ) THEN
C
C ............ 粘性項
               GZ1 = GZ(I,J,K)*XC(8,I,J)+GZ(I+1,J,K)*XC(7,I,J)
               TMU0 = TMU(I,J,KNB1)*XC(7,I,J)+TMU(I+1,J,KNB1)*XC(8,I,J)
               VIS1 = GZ1*(ANUV+TMU0)*(WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
C
C ............ 慣性項
               HW1 = HW(I,J,K)*XC(8,I,J)+HW(I+1,J,K)*XC(7,I,J)
               ADV1 = UU(I,J,KNB1)*HW1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDW1.EQ.-1 .AND. INDW2.EQ.-1 ) THEN
C
C ............ 粘性項
               GZ1 = GZ(I,J,K)*XC(8,I,J)+GZ(I+1,J,K)*XC(7,I,J)
               TMU0 = TMU(I,J,KNB1)*XC(7,I,J)+TMU(I+1,J,KNB1)*XC(8,I,J)
               UU1 = UU(I,J,KNB1)
               UU2 = UU(I,J,KNB1)
cdist          UU1 = 0.25D0*UU(I-1,J,KNB1)+0.75D0*UU(I  ,J,KNB1)
cdist          UU2 = 0.75D0*UU(I  ,J,KNB1)+0.25D0*UU(I+1,J,KNB1)
C
               VIS1 = (ANUV+TMU0)
     $              *(GZ1*(WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
     $              + GZ(I  ,J,K)*XC(8,I,J)*(UUZ(I  ,J,K)-UU1)*ZC1
     $              + GZ(I+1,J,K)*XC(7,I,J)*(UUZ(I+1,J,K)-UU2)*ZC1)
C
C ............ 慣性項
               ADV1 = UUZ(I  ,J,K)*HW(I  ,J,K)*XC(8,I,J)
     $              + UUZ(I+1,J,K)*HW(I+1,J,K)*XC(7,I,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：I,KNB1セルとI+1,KNB1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDW1.GT.0 ) THEN
                  TMU0 = TMU(I,J,K)*ZC(7,K)+TMU(I,J,K+1)*ZC(8,K)
                  VIS1M = GZ(I,J,K)*(ANUV+TMU0)
     $                  *((UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
     $                  + (WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J))
C
                  UU1 = UU(I,J,K)*ZC(7,K)+UU(I,J,K+1)*ZC(8,K)
                  ADV1M = PARAMV2* UU1*HW(I,J,K)
     $                  + PARAMV *(UU(I,J,K  )*MAX(HW(I,J,K),0.0D0)
     $                            +UU(I,J,K+1)*MIN(HW(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDW1.EQ.0 ) THEN
                  TMU0 = TMU(I,J,KNB1)
                  VIS1M = GZ(I,J,K)*(ANUV+TMU0)
     $                  *(WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
C
                  ADV1M = UU(I,J,KNB1)*HW(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW1.EQ.-1 .OR. INDW1.EQ.-2 ) THEN
                  UU1 = UU(I,J,KNB1)
cdist             UU1 = 0.25D0*UU(I-1,J,KNB1)+0.75D0*UU(I,J,KNB1)
                  IF( UUZ(I,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 = TMU(I,J,KNB1)
                  VIS1M = GZ(I,J,K)*(ANUV+TMU0)
     $                  *((UUZ(I,J,K)-UU1)*ZC1
     $                  + (WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J))
C
                  ADV1M = UUZ(I,J,K)*HW(I,J,K)
C
               ELSE
                  VIS1M = 0.0D0
                  ADV1M = 0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDW2.GT.0 ) THEN
                  TMU0 = TMU(I+1,J,K)*ZC(7,K)+TMU(I+1,J,K+1)*ZC(8,K)
                  VIS1P = GZ(I+1,J,K)*(ANUV+TMU0)
     $                  *((UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
     $                  + (WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J))
C
                  UU1 = UU(I,J,K)*ZC(7,K)+UU(I,J,K+1)*ZC(8,K)
                  ADV1P = PARAMV2* UU1*HW(I+1,J,K)
     $                  + PARAMV *(UU(I,J,K  )*MAX(HW(I+1,J,K),0.0D0)
     $                            +UU(I,J,K+1)*MIN(HW(I+1,J,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDW2.EQ.0 ) THEN
                  TMU0 = TMU(I+1,J,KNB1)
                  VIS1P = GZ(I+1,J,K)*(ANUV+TMU0)
     $                  *(WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
C
                  ADV1P = UU(I,J,KNB1)*HW(I+1,J,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW2.EQ.-1 .OR. INDW2.EQ.-2 ) THEN
                  UU1 = UU(I,J,KNB1)
cdist             UU1 = 0.75D0*UU(I,J,KNB1)+0.25D0*UU(I+1,J,KNB1)
                  IF( UUZ(I+1,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 = TMU(I+1,J,KNB1)
                  VIS1P = GZ(I+1,J,K)*(ANUV+TMU0)
     $                  *((UUZ(I+1,J,K)-UU1)*ZC1
     $                  + (WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J))
C
                  ADV1P = UUZ(I+1,J,K)*HW(I+1,J,K)
C
               ELSE
                  VIS1P = 0.0D0
                  ADV1P = 0.0D0
               END IF
C
C
C ............ (4.7) 海底摩擦
               IF( IGM2B.NE.0 .AND.K.EQ.MAX(KG(I,J),KG(I+1,J))-1 ) THEN
C
C              (a) 段差なし
               IF( KG(I,J).EQ.KG(I+1,J) ) THEN
                 VVUU = 0.5D0*(VV(I  ,J-1,K+1)+VV(I  ,J,K+1))*XC(7,I,J)
     $                + 0.5D0*(VV(I+1,J-1,K+1)+VV(I+1,J,K+1))*XC(8,I,J)
                 UUUU = UU(I,J,K+1)
                 VABS = DSQRT(UUUU**2+VVUU**2)
C
                 IF( IGM2B.EQ.1 ) THEN
                   VIS1M = GM2B*UUUU*VABS
                   VIS1P = GM2B*UUUU*VABS
                 ELSE IF( IGM2B.EQ.-1 ) THEN
                   DH1 = HH(I  ,J)-HDEP(I  ,J)
                   DH2 = HH(I+1,J)-HDEP(I+1,J)
                   DHH = DH1*XC(8,I,J)+DH2*XC(7,I,J)
                   DHH = MAX(DHH,GXB*0.1D0)
                   BMNG = XC(8,I,J)*AMNG(I,J)+XC(7,I,J)*AMNG(I+1,J)
C
                   IF(BMNG*VABS.NE.0.0D0) THEN
                     CEXP = DHH**(4.0D0/3.0D0)/(DT*(-GRAV)*BMNG**2*VABS)
                     CEXP = MIN(CEXP,REXP)
                     CIMP = 1.0D0-CEXP
                   ELSE
                     CEXP = 0.0D0
                     CIMP = 0.0D0
                   ENDIF
C
                   IF(DHH.LT.GXB) THEN
                     RES1 = 0.0D0
                     RES2 = 0.0D0
                   ELSE
                     RES1 = - CEXP*GRAV*BMNG**2/DHH**(1.0D0/3.0D0)
                     RES2 = - CIMP*GRAV*BMNG**2/DHH**(1.0D0/3.0D0)
                   END IF
C
                   VIS1M = RES1*UUUU*VABS
                   VIS1P = RES1*UUUU*VABS
                   DIMP(I,J,K) = DIMP(I,J,K)+ RES2*VABS
                 END IF
C
C ............ (b) 右下がり地形
               ELSE IF( KG(I,J).GT.KG(I+1,J) ) THEN
                 VVUU = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
                 UUUU = UU(I,J,K+1)
                 VABS = DSQRT(UUUU**2+VVUU**2)
C
                 IF( IGM2B.EQ.1 ) THEN
                   VIS1M = GM2B*UUUU*VABS
                 ELSE IF( IGM2B.EQ.-1 ) THEN
                   DHH = HH(I,J)-HDEP(I,J)
                   DHH = MAX(DHH,GXB*0.1D0)
                   BMNG = AMNG(I,J)
C
                   IF(BMNG*VABS.NE.0.0D0) THEN
                     CEXP = DHH**(4.0D0/3.0D0)/(DT*(-GRAV)*BMNG**2*VABS)
                     CEXP = MIN(CEXP,REXP)
                     CIMP = 1.0D0-CEXP
                   ELSE
                     CEXP = 0.0D0
                     CIMP = 0.0D0
                   ENDIF
C
                   IF(DHH.LT.GXB) THEN
                     RES1 = 0.0D0
                     RES2 = 0.0D0
                   ELSE
                     RES1 = - CEXP*GRAV*BMNG**2/DHH**(1.0D0/3.0D0)
                     RES2 = - CIMP*GRAV*BMNG**2/DHH**(1.0D0/3.0D0)
                   END IF
C
                   VIS1M = RES1*UUUU*VABS
                   DIMP(I,J,K) = DIMP(I,J,K) + XC(8,I,J)*RES2*VABS
                 ENDIF
C
C              (c) 右上り地形
               ELSE IF( KG(I,J).LT.KG(I+1,J) ) THEN
                 VVUU = 0.5D0*(VV(I+1,J-1,K+1)+VV(I+1,J,K+1))
                 UUUU = UU(I,J,K+1)
                 VABS = DSQRT(UUUU**2+VVUU**2)
C
                 IF( IGM2B.EQ.1 ) THEN
                   VIS1P = GM2B*UUUU*DSQRT(UUUU**2+VVUU**2)
                 ELSE IF( IGM2B.EQ.-1 ) THEN
                   DHH = HH(I+1,J)-HDEP(I+1,J)
                   DHH = MAX(DHH,GXB*0.1D0)
                   BMNG = AMNG(I+1,J)
C
                   IF(BMNG*VABS.NE.0.0D0) THEN
                     CEXP = DHH**(4.0D0/3.0D0)/(DT*(-GRAV)*BMNG**2*VABS)
                     CEXP = MIN(CEXP,REXP)
                     CIMP = 1.0D0-CEXP
                   ELSE
                     CEXP = 0.0D0
                     CIMP = 0.0D0
                   ENDIF
C
                   IF(DHH.LT.GXB) THEN
                     RES1 = 0.0D0
                     RES2 = 0.0D0
                   ELSE
                     RES1 = - CEXP*GRAV*BMNG**2/DHH**(1.0D0/3.0D0)
                     RES2 = - CIMP*GRAV*BMNG**2/DHH**(1.0D0/3.0D0)
                   END IF
C
                   VIS1P = RES1*UUUU*VABS
                   DIMP(I,J,K) = DIMP(I,J,K) + XC(7,I,J)*RES2*VABS
                 END IF
               END IF
            END IF
C
               VIS1 = VIS1M*XC(8,I,J) + VIS1P*XC(7,I,J)
               ADV1 = ADV1M*XC(8,I,J) + ADV1P*XC(7,I,J)
            END IF
C
            IF(ISW(4).NE.0) ADV1=0.0D0
            IF(ISW(3).EQ.1.AND.IWIND.NE.0.AND.K.EQ.KF1-1) VIS1=0.0D0
            FW(I,J,K) = VIS1 - ADV1
          END IF
          END IF
C
  100 CONTINUE
C
C
C ... (5) 表面風摩擦
      IF( IGM2S.NE.0.OR.ISURF(1).LE.-1 ) THEN
      DO 200 J=2,MYM
C     DO 200 I=2,MXM-1
C                   ( for DOMAIN-DECOMP )
      DO 200 I=2,MXM
         K = MAX(KF(I,J),KF(I+1,J))
         IF( INDU(I,J,K).GT.0 ) THEN
C
            WXUU = WX(I,J)*XC(7,I,J)+WX(I+1,J)*XC(8,I,J)
            WYUU = WY(I,J)*XC(7,I,J)+WY(I+1,J)*XC(8,I,J)
            WABS = DSQRT(WXUU**2+WYUU**2)
C
            IF(ISURF(1).EQ.-1) THEN
               CDX = XC(8,I,J)*CD(I,J)+XC(7,I,J)*CD(I+1,J)
               IF( CDX.LT.1.0D-10 ) THEN
                  IF(WABS.LT.8.0D0) THEN
                     CDX = 0.001D0*(1.290D0-0.024D0*WABS)
                  ELSE
                     CDX = 0.001D0*(0.581D0+0.063D0*WABS)
                  ENDIF
               ENDIF
               TAU1 = CDX*ADRHO*WXUU*WABS
            ELSE IF( IGM2S.EQ.1 ) THEN
               TAU1 = GM2S*ADRHO*WXUU*DSQRT(WXUU**2+WYUU**2)
            ELSE
C ......... (風速の関数化)
               IF(WABS.LT.8.0D0) THEN
                  CDX = 0.001D0*(1.290D0-0.024D0*WABS)
               ELSE
                  CDX = 0.001D0*(0.581D0+0.063D0*WABS)
               END IF
               TAU1 = CDX*ADRHO*WXUU*WABS
            END IF
C
            HH1 = MAX(FF(I  ,J,K)-1.0D0+GV0(I  ,J,K),0.0D0)
     $         *GV(I  ,J,K)/GV0(I  ,J,K)
            HH2 = MAX(FF(I+1,J,K)-1.0D0+GV0(I+1,J,K),0.0D0)
     $         *GV(I+1,J,K)/GV0(I+1,J,K)
            HH0 = HH1*XC(8,I,J)+HH2*XC(7,I,J)
C
            IF( MZ.EQ.3 ) THEN
               DU(I,J,K) = DU(I,J,K) + TAU1*ZC(6,K)
            ELSE IF( K.EQ.KG(I,J).OR.K.EQ.KG(I+1,J) ) THEN
               IF( HH0.GE.GZH*ZC(6,K) )
     $            DU(I,J,K) = DU(I,J,K) + TAU1*ZC(6,K)
            ELSE IF( ISW(3).NE.0 ) THEN
               DU(I,J,K-1) = DU(I,J,K-1) + TAU1*ZC(6,K-1)
            ELSE
               HH1 = MAX(FF(I  ,J,K-1)-1.0D0+GV(I  ,J,K-1),0.0D0)
               HH2 = MAX(FF(I+1,J,K-1)-1.0D0+GV(I+1,J,K-1),0.0D0)
               HHB = HH1*XC(8,I,J)+HH2*XC(7,I,J)
C
               COEF0 = HH0*ZC(4,K)/(HH0*ZC(4,K)+HHB*ZC(4,K-1))
               COEF1 = 1.0D0-COEF0
               IF( HH0.GE.GZH*ZC(6,K) )
     $            DU(I,J,K  ) = DU(I,J,K  ) + COEF0*TAU1*ZC(6,K)
               IF( HHB.GE.GZH*ZC(6,K-1) )
     $            DU(I,J,K-1) = DU(I,J,K-1) + COEF1*TAU1*ZC(6,K-1)
C              (下側に板境界がある場合はDU(I,J,K)のみ加える：不安定回避目的)
            END IF
         END IF
  200 CONTINUE
      END IF
C
      RETURN
      END
