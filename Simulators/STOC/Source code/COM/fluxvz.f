      SUBROUTINE FLUXVZ(FW,UU,VV,WW,HW,VVZ,FF,TMU,YC,ZC,GV,GZ,GV0,HH,
     $                  HDEP,DV,DIMP,AMNG,WX,WY,CD,INDV,INDW,KG,KP,KF)
C======================================================================
C     Y方向の運動量保存式のZ方向界面の運動量流束を計算する
C     FW: X方向セル中心、Y方向格子点、Z方向格子点で定義
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
      REAL(8),INTENT(INOUT)::HW(MX,MY,MZ),VVZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::YC(8,MY),ZC(8,MZ),GV(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::DV(MX,MY,MZ),DIMP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AMNG(MX,MY),WX(MX,MY),WY(MX,MY),CD(MX,MY)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KG(MX,MY),KP(MX,MY),KF(MX,MY)
C
      REAL(8)::ADV1,ADV1M,ADV1P,VIS1,VIS1M,VIS1P
      REAL(8)::VV1,VV2,GZ1,HW1,TMU0,TMU1,TMU2,ZC1
      REAL(8)::DHH,DH1,DH2,BMNG,CDY,CEXP,CIMP,RES1,RES2,REXP
      REAL(8)::UUVV,VVVV,WXVV,WYVV,VABS,WABS,TAU1
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
C     DO 100 J=2,MYM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
         KP1 = MAX(KP(I,J),KP(I,J+1))
         KF1 = MIN(MAX(KF(I,J),KF(I,J+1)),MZM)
         IF( LSURF.EQ.0 ) KF1=MZM+1
         IF( K.LE.KF1-1 ) THEN
C
C ...... ±Zのどちらかの側が流速のY方向成分計算点である
         IF( INDV(I,J,K).GT.0 .OR. INDV(I,J,K+1).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDV(I,J,K).GT.0 ) THEN
               KNB1 = K
               ZC1  = 2.0D0*ZC(6,K)
            ELSE
               KNB1 = K+1
               ZC1  = -2.0D0*ZC(6,K+1)
            END IF
C
            INDW1 = INDW(I,J  ,K)
            INDW2 = INDW(I,J+1,K)
C
C ......... (1) 境界条件なし
            IF( INDW1.GT.0 .AND. INDW2.GT.0 ) THEN
C
C ............ 粘性項
               GZ1  = GZ(I,J,K)*YC(8,J)+GZ(I,J+1,K)*YC(7,J)
               TMU1 = TMU(I,J,K  )*YC(7,J)+TMU(I,J+1,K  )*YC(8,J)
               TMU2 = TMU(I,J,K+1)*YC(7,J)+TMU(I,J+1,K+1)*YC(8,J)
               TMU0 = TMU1*ZC(7,K)+TMU2*ZC(8,K)
               VIS1 = GZ1*(ANUV+TMU0)*((VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
     $              +                  (WW(I,J+1,K)-WW(I,J,K))*YC(5,J))
C
C ............ 慣性項
               VV1 = VV(I,J,K)*ZC(7,K)+VV(I,J,K+1)*ZC(8,K)
               HW1 = HW(I,J,K)*YC(8,J)+HW(I,J+1,K)*YC(7,J)
               ADV1 = PARAMV2* VV1*HW1
     $              + PARAMV *(VV(I,J,K  )*MAX(HW1,0.0D0)
     $                        +VV(I,J,K+1)*MIN(HW1,0.0D0))
C
               IF( IMPSRF.EQ.1.AND.K.EQ.KF1-1 ) THEN
                  HH1 = MAX(FF(I,J  ,K+1)-1.0D0+GV0(I,J  ,K+1),0.0D0)
     $               *GV(I,J  ,K+1)/GV0(I,J  ,K+1)
                  HH2 = MAX(FF(I,J+1,K+1)-1.0D0+GV0(I,J+1,K+1),0.0D0)
     $               *GV(I,J+1,K+1)/GV0(I,J+1,K+1)
                  HH0 = HH1*YC(8,J)+HH2*YC(7,J)
                  COEF0 = GZ1*(ANUV+TMU0)*(1.0D0-HH0)*ZC(5,K)*ZC(6,K+1)
                  DV(I,J,K+1) = DV(I,J,K+1) + COEF0*VV(I,J,K+1)
                  DIMP(I,J,K+1) = DIMP(I,J,K+1) + COEF0
               END IF
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDW1.EQ.0 .AND. INDW2.EQ.0 ) THEN
C
C ............ 粘性項
               GZ1 = GZ(I,J,K)*YC(8,J)+GZ(I,J+1,K)*YC(7,J)
               TMU0 = TMU(I,J,KNB1)*YC(7,J)+TMU(I,J+1,KNB1)*YC(8,J)
               VIS1 = GZ1*(ANUV+TMU0)*(WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
C
C ............ 慣性項
               HW1 = HW(I,J,K)*YC(8,J)+HW(I,J+1,K)*YC(7,J)
               ADV1 = VV(I,J,KNB1)*HW1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDW1.EQ.-1 .AND. INDW2.EQ.-1 ) THEN
C
C ............ 粘性項
               GZ1 = GZ(I,J,K)*YC(8,J)+GZ(I,J+1,K)*YC(7,J)
               TMU0 = TMU(I,J,KNB1)*YC(7,J)+TMU(I,J+1,KNB1)*YC(8,J)
               VV1 = VV(I,J,KNB1)
               VV2 = VV(I,J,KNB1)
cdist          VV1 = 0.25D0*VV(I,J-1,KNB1)+0.75D0*VV(I,J  ,KNB1)
cdist          VV2 = 0.75D0*VV(I,J  ,KNB1)+0.25D0*VV(I,J+1,KNB1)
C
               VIS1 = (ANUV+TMU0)
     $              *(GZ1*(WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
     $              + GZ(I,J  ,K)*YC(8,J)*(VVZ(I,J  ,K)-VV1)*ZC1
     $              + GZ(I,J+1,K)*YC(7,J)*(VVZ(I,J+1,K)-VV2)*ZC1)
C
C ............ 慣性項
               ADV1 = VVZ(I,J  ,K)*HW(I,J  ,K)*YC(8,J)
     $              + VVZ(I,J+1,K)*HW(I,J+1,K)*YC(7,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：J,KNB1セルとJ+1,KNB1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDW1.GT.0 ) THEN
                  TMU0 = TMU(I,J,K)*ZC(7,K)+TMU(I,J,K+1)*ZC(8,K)
                  VIS1M = GZ(I,J,K)*(ANUV+TMU0)
     $                  *((VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
     $                  + (WW(I,J+1,K)-WW(I,J,K))*YC(5,J))
C
                  VV1 = VV(I,J,K)*ZC(7,K)+VV(I,J,K+1)*ZC(8,K)
                  ADV1M = PARAMV2* VV1*HW(I,J,K)
     $                  + PARAMV *(VV(I,J,K  )*MAX(HW(I,J,K),0.0D0)
     $                            +VV(I,J,K+1)*MIN(HW(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDW1.EQ.0 ) THEN
                  TMU0 = TMU(I,J,KNB1)
                  VIS1M = GZ(I,J,K)*(ANUV+TMU0)
     $                  *(WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
C
                  ADV1M = VV(I,J,KNB1)*HW(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW1.EQ.-1 .OR. INDW1.EQ.-2 ) THEN
                  VV1 = VV(I,J,KNB1)
cdist             VV1 = 0.25D0*VV(I,J-1,KNB1)+0.75D0*VV(I,J,KNB1)
                  IF( VVZ(I,J,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 = TMU(I,J,KNB1)
                  VIS1M = GZ(I,J,K)*(ANUV+TMU0)
     $                  *((VVZ(I,J,K)-VV1)*ZC1
     1                  + (WW(I,J+1,K)-WW(I,J,K))*YC(5,J))
C
                  ADV1M = VVZ(I,J,K)*HW(I,J,K)
C
               ELSE
                  VIS1M = 0.0D0
                  ADV1M = 0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDW2.GT.0 ) THEN
                  TMU0 = TMU(I,J+1,K)*ZC(7,K)+TMU(I,J+1,K+1)*ZC(8,K)
                  VIS1P = GZ(I,J+1,K)*(ANUV+TMU0)
     $                  *((VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
     $                  + (WW(I,J+1,K)-WW(I,J,K))*YC(5,J))
C
                  VV1 = VV(I,J,K)*ZC(7,K)+VV(I,J,K+1)*ZC(8,K)
                  ADV1P = PARAMV2* VV1*HW(I,J+1,K)
     $                  + PARAMV *(VV(I,J,K  )*MAX(HW(I,J+1,K),0.0D0)
     $                            +VV(I,J,K+1)*MIN(HW(I,J+1,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDW2.EQ.0 ) THEN
                  TMU0 = TMU(I,J+1,KNB1)
                  VIS1P = GZ(I,J+1,K)*(ANUV+TMU0)
     $                  *(WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
C
                  ADV1P = VV(I,J,KNB1)*HW(I,J+1,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW2.EQ.-1 .OR. INDW2.EQ.-2 ) THEN
                  VV1 = VV(I,J,KNB1)
cdist             VV1 = 0.75D0*VV(I,J,KNB1)+0.25D0*VV(I,J+1,KNB1)
                  IF( VVZ(I,J+1,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 = TMU(I,J+1,KNB1)
                  VIS1P = GZ(I,J+1,K)*(ANUV+TMU0)
     $                  *((VVZ(I,J+1,K)-VV1)*ZC1
     1                  + (WW(I,J+1,K)-WW(I,J,K))*YC(5,J))
C
                  ADV1P = VVZ(I,J+1,K)*HW(I,J+1,K)
C
               ELSE
                  VIS1P = 0.0D0
                  ADV1P = 0.0D0
               END IF
C
C
C ............ (4.7) 海底摩擦
               IF( IGM2B.NE.0 .AND.K.EQ.MAX(KG(I,J),KG(I,J+1))-1 ) THEN
C
C              (a) 段差なし
               IF( KG(I,J).EQ.KG(I,J+1) ) THEN
                 UUVV = 0.5D0*(UU(I-1,J  ,K+1)+UU(I,J  ,K+1))*YC(7,J)
     1                + 0.5D0*(UU(I-1,J+1,K+1)+UU(I,J+1,K+1))*YC(8,J)
                 VVVV = VV(I,J,K+1)
                 VABS = DSQRT(UUVV**2+VVVV**2)
C
                 IF( IGM2B.EQ.1 ) THEN
                   VIS1M = GM2B*VVVV*VABS
                   VIS1P = GM2B*VVVV*VABS
                 ELSE IF( IGM2B.EQ.-1 ) THEN
                   DH1 = HH(I,J)-HDEP(I,J)
                   DH2 = HH(I,J+1)-HDEP(I,J+1)
                   DHH = DH1*YC(8,J)+DH2*YC(7,J)
                   DHH = MAX(DHH,GXB*0.1D0)
                   BMNG = YC(8,J)*AMNG(I,J)+YC(7,J)*AMNG(I,J+1)
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
                   VIS1M = RES1*VVVV*VABS
                   VIS1P = RES1*VVVV*VABS
                   DIMP(I,J,K) = DIMP(I,J,K) + RES2*VABS
                 END IF
C
C              (b) 右下がり地形
               ELSE IF( KG(I,J).GT.KG(I,J+1) ) THEN
                 UUVV = 0.5D0*(UU(I-1,J,K+1)+UU(I,J,K+1))
                 VVVV = VV(I,J,K+1)
                 VABS = DSQRT(UUVV**2+VVVV**2)
C
                 IF( IGM2B.EQ.1 ) THEN
                   VIS1M = GM2B*VVVV*VABS
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
                   VIS1M = RES1*VVVV*VABS
                   DIMP(I,J,K) = DIMP(I,J,K) + YC(8,J)*RES2*VABS
                 END IF
C
C              (c) 右上り地形
               ELSE IF( KG(I,J).LT.KG(I,J+1) ) THEN
                 UUVV = 0.5D0*(UU(I-1,J+1,K+1)+UU(I,J+1,K+1))
                 VVVV = VV(I,J,K+1)
                 VABS = DSQRT(UUVV**2+VVVV**2)
C
                 IF( IGM2B.EQ.1 ) THEN
                   VIS1P = GM2B*VVVV*DSQRT(UUVV**2+VVVV**2)
                 ELSE IF( IGM2B.EQ.-1 ) THEN
                   DHH = HH(I,J+1)-HDEP(I,J+1)
                   DHH = MAX(DHH,GXB*0.1D0)
                   BMNG = AMNG(I,J+1)
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
                   VIS1P = RES1*VVVV*VABS
                   DIMP(I,J,K) = DIMP(I,J,K) + YC(7,J)*RES2*VABS
                 END IF
               END IF
               END IF
C
               VIS1 = VIS1M*YC(8,J) + VIS1P*YC(7,J)
               ADV1 = ADV1M*YC(8,J) + ADV1P*YC(7,J)
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
C     DO 200 J=2,MYM-1
C                   ( for DOMAIN-DECOMP )
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         K = MAX(KF(I,J),KF(I,J+1))
         IF( INDV(I,J,K).GT.0 ) THEN

            WXVV = WX(I,J)*YC(7,J)+WX(I,J+1)*YC(8,J)
            WYVV = WY(I,J)*YC(7,J)+WY(I,J+1)*YC(8,J)
            WABS = DSQRT(WXVV**2+WYVV**2)
C
            IF(ISURF(1).EQ.-1) THEN
               CDY = YC(8,J)*CD(I,J)+YC(7,J)*CD(I,J+1)
               IF( CDY.LT.1.0D-10 ) THEN
                  IF(WABS.LT.8.0D0) THEN
                     CDY = 0.001D0*(1.290D0-0.024D0*WABS)
                  ELSE
                     CDY = 0.001D0*(0.581D0+0.063D0*WABS)
                  ENDIF
               ENDIF
               TAU1 = CDY*ADRHO*WYVV*WABS
            ELSE IF( IGM2S.EQ.1 ) THEN
               TAU1 = GM2S*ADRHO*WYVV*DSQRT(WXVV**2+WYVV**2)
            ELSE
C ......... (風速の関数化)
               IF(WABS.LT.8.0D0) THEN
                  CDY = 0.001D0*(1.290D0-0.024D0*WABS)
               ELSE
                  CDY = 0.001D0*(0.581D0+0.063D0*WABS)
               END IF
               TAU1 = CDY*ADRHO*WYVV*WABS
            END IF
C
            HH1 = MAX(FF(I,J  ,K)-1.0D0+GV0(I,J  ,K),0.0D0)
     $         *GV(I,J  ,K)/GV0(I,J  ,K)
            HH2 = MAX(FF(I,J+1,K)-1.0D0+GV0(I,J+1,K),0.0D0)
     $         *GV(I,J+1,K)/GV0(I,J+1,K)
            HH0 = HH1*YC(8,J)+HH2*YC(7,J)
C
            IF( MZ.EQ.3 ) THEN
               DV(I,J,K) = DV(I,J,K) + TAU1*ZC(6,K)
            ELSE IF( K.EQ.KG(I,J).OR.K.EQ.KG(I,J+1) ) THEN
               IF( HH0.GE.GZH*ZC(6,K) )
     $            DV(I,J,K) = DV(I,J,K) + TAU1*ZC(6,K)
            ELSE IF( ISW(3).NE.0 ) THEN
               DV(I,J,K-1) = DV(I,J,K-1) + TAU1*ZC(6,K-1)
            ELSE
               HH1 = MAX(FF(I,J  ,K-1)-1.0D0+GV0(I,J  ,K-1),0.0D0)
     $            *GV(I,J  ,K-1)/GV0(I,J  ,K-1)
               HH2 = MAX(FF(I,J+1,K-1)-1.0D0+GV0(I,J+1,K-1),0.0D0)
     $            *GV(I,J+1,K-1)/GV0(I,J+1,K-1)
               HHB = HH1*YC(8,J)+HH2*YC(7,J)
C
               COEF0 = HH0*ZC(4,K)/(HH0*ZC(4,K)+HHB*ZC(4,K-1))
               COEF1 = 1.0D0-COEF0
               IF( HH0.GE.GZH*ZC(6,K) )
     $            DV(I,J,K  ) = DV(I,J,K  ) + COEF0*TAU1*ZC(6,K)
               IF( HHB.GE.GZH*ZC(6,K-1) )
     $            DV(I,J,K-1) = DV(I,J,K-1) + COEF1*TAU1*ZC(6,K-1)
C              (下側に板境界がある場合はDV(I,J,K)のみ加える：不安定回避目的)
            END IF
         END IF
  200 CONTINUE
      END IF
C
      RETURN
      END
