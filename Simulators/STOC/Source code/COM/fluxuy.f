      SUBROUTINE FLUXUY(FV,UU,VV,HV,UUY,FF,TMU,XC,YC,XCP,GV,GY,GV0,GY0,
     $                  INDU,INDV,KF)
C======================================================================
C     X方向の運動量保存式のY方向界面の運動量流束を計算する
C     FV: X方向格子点、Y方向格子点、Z方向セル中心で定義
C
C     2つのセル面にまたがる界面の場合分け
C     (1) 境界条件なし
C     (2) 両方とも自由流入出境界
C     (3) 両方とも速度固定出境界
C     (4) 壁境界または2種類の境界条件が存在
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::FV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UUY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),XCP(8,MX,MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GY0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ),KF(MX,MY)
C
      REAL(8)::YC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::TMU1,TMU2,TMU0,UU1,UU2,HV1
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDV1,INDV2,JNB1
C
C
      CALL ZERCLR(FV,MXYZ,0.0D0)
C
      DO 100 K=2,MZM
      DO 100 J=1,MYM
C     DO 100 I=2,MXM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 I=2,MXM
C
         KF1 = MAX(KF(I,J),KF(I+1,J),KF(I,J+1),KF(I+1,J+1))
         KF1 = MIN(KF1,MZM)
         IF( K.LE.KF1 ) THEN
C
C ...... ±Yのどちらかの側が流速のX方向成分計算点である
         IF( INDU(I,J,K).GT.0 .OR. INDU(I,J+1,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDU(I,J,K).GT.0 ) THEN
               JNB1 = J
               YC1  = 2.0D0*YC(6,J)
            ELSE
               JNB1 = J+1
               YC1  = -2.0D0*YC(6,J+1)
            END IF
C
            INDV1 = INDV(I  ,J,K)
            INDV2 = INDV(I+1,J,K)
C
C ......... 各メッシュの層厚比を計算しておく
cxxxxxxxxx  FF1 = FF(I  ,J,K)*YC(7,J)+FF(I  ,J+1,K)*YC(8,J)
cxxxxxxxxx  FF2 = FF(I+1,J,K)*YC(7,J)+FF(I+1,J+1,K)*YC(8,J)
C ......... 壁面でのFFは勾配0条件と仮定
cxxxxxxxxxx IF( INDV1.EQ.-2 ) FF1 = FF(I  ,JNB1,K)
cxxxxxxxxxx IF( INDV2.EQ.-2 ) FF2 = FF(I+1,JNB1,K)
            FF1 = MIN(FF(I  ,J,K),FF(I  ,J+1,K))
            FF2 = MIN(FF(I+1,J,K),FF(I+1,J+1,K))
C
            HH1 = MAX(FF1-1.0D0+GY0(I  ,J,K),0.0D0)
     $          * GY(I,J,K)/GY0(I,J,K)
            HH2 = MAX(FF2-1.0D0+GY0(I+1,J,K),0.0D0)
     $          * GY(I+1,J,K)/GY0(I+1,J,K)
            IF( INDV1.EQ.-2 ) HH1 = MAX(FF1-1.0D0+GV0(I  ,JNB1,K),0.0D0)
     $                            *GV(I,JNB1,K)/GV0(I,JNB1,K)
            IF( INDV2.EQ.-2 ) HH2 = MAX(FF2-1.0D0+GV0(I+1,JNB1,K),0.0D0)
     $                            *GV(I+1,JNB1,K)/GV0(I+1,JNB1,K)
            HH0 = HH1*XCP(8,I,J)+HH2*XCP(7,I,J)
C
C ......... (1) 境界条件なし
            IF( INDV1.GT.0 .AND. INDV2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1 =TMU(I,J  ,K)*XC(7,I,J  )+TMU(I+1,J  ,K)*XC(8,I,J  )
               TMU2 =TMU(I,J+1,K)*XC(7,I,J+1)+TMU(I+1,J+1,K)*XC(8,I,J+1)
               TMU0 =TMU1*YC(7,J)+TMU2*YC(8,J)
               VIS1 = HH0*(ANUH+TMU0)*((UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
     $              +                (VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J))
C
C ............ 慣性項
               UU1 = UU(I,J,K)*YC(7,J)+UU(I,J+1,K)*YC(8,J)
               HV1 = HV(I,J,K)*XCP(8,I,J)+HV(I+1,J,K)*XCP(7,I,J)
               ADV1 = PARAMV2* UU1*HV1
     $              + PARAMV *(UU(I,J  ,K)*MAX(HV1,0.0D0)
     $                        +UU(I,J+1,K)*MIN(HV1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDV1.EQ.0 .AND. INDV2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(I  ,JNB1,K)*XC(7,I,JNB1)
     $               +TMU(I+1,JNB1,K)*XC(8,I,JNB1)
               VIS1 = HH0*(ANUH+TMU0)*(VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
C
C ............ 慣性項
               HV1 = HV(I,J,K)*XCP(8,I,J)+HV(I+1,J,K)*XCP(7,I,J)
               ADV1 = UU(I,JNB1,K)*HV1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDV1.EQ.-1 .AND. INDV2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(I  ,JNB1,K)*XC(7,I,JNB1)
     $               +TMU(I+1,JNB1,K)*XC(8,I,JNB1)
               UU1  = UU(I,JNB1,K)
               UU2  = UU(I,JNB1,K)
cdist          UU1  = 0.25D0*UU(I-1,JNB1,K)+0.75D0*UU(I  ,JNB1,K)
cdist          UU2  = 0.75D0*UU(I  ,JNB1,K)+0.25D0*UU(I+1,JNB1,K)
C
               VIS1 = (ANUH+TMU0)
     $              * (HH0        *(VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
     $              +  HH1*XCP(8,I,J)*(UUY(I  ,J,K)-UU1)*YC1
     $              +  HH2*XCP(7,I,J)*(UUY(I+1,J,K)-UU2)*YC1)
C
C ............ 慣性項
               ADV1 = UUY(I  ,J,K)*HV(I  ,J,K)*XCP(8,I,J)
     $              + UUY(I+1,J,K)*HV(I+1,J,K)*XCP(7,I,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：I,JNB1セルとI+1,JNB1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDV1.GT.0 ) THEN
                  TMU0 = TMU(I,J,K)*YC(7,J)+TMU(I,J+1,K)*YC(8,J)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
     $                  + (VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J))
C
                  UU1 = UU(I,J,K)*YC(7,J)+UU(I,J+1,K)*YC(8,J)
                  ADV1M = PARAMV2* UU1*HV(I,J,K)
     $                  + PARAMV *(UU(I,J  ,K)*MAX(HV(I,J,K),0.0D0)
     $                            +UU(I,J+1,K)*MIN(HV(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDV1.EQ.0 ) THEN
                  TMU0 = TMU(I,JNB1,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *(VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
C
                  ADV1M = UU(I,JNB1,K)*HV(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV1.EQ.-1 .OR. INDV1.EQ.-2 ) THEN
                  UU1 = UU(I,JNB1,K)
cdist             UU1 = 0.25D0*UU(I-1,JNB1,K)+0.75D0*UU(I,JNB1,K)
                  IF( UUY(I,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 = TMU(I,JNB1,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((UUY(I,J,K)-UU1)*YC1
     $                  + (VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J))
C
                  ADV1M = UUY(I,J,K)*HV(I,J,K)
C
               ELSE
                  VIS1M = 0.0D0
                  ADV1M = 0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDV2.GT.0 ) THEN
                  TMU0 = TMU(I+1,J,K)*YC(7,J)+TMU(I+1,J+1,K)*YC(8,J)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
     $                  + (VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J))
C
                  UU1 = UU(I,J,K)*YC(7,J)+UU(I,J+1,K)*YC(8,J)
                  ADV1P = PARAMV2* UU1*HV(I+1,J,K)
     $                  + PARAMV *(UU(I,J  ,K)*MAX(HV(I+1,J,K),0.0D0)
     $                            +UU(I,J+1,K)*MIN(HV(I+1,J,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDV2.EQ.0 ) THEN
                  TMU0 = TMU(I+1,JNB1,K)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *(VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
C
                  ADV1P = UU(I,JNB1,K)*HV(I+1,J,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV2.EQ.-1 .OR. INDV2.EQ.-2 ) THEN
                  UU1 = UU(I,JNB1,K)
cdist             UU1 = 0.75D0*UU(I,JNB1,K)+0.25D0*UU(I+1,JNB1,K)
                  IF( UUY(I+1,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 = TMU(I+1,JNB1,K)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((UUY(I+1,J,K)-UU1)*YC1
     $                  + (VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J))
C
                  ADV1P = UUY(I+1,J,K)*HV(I+1,J,K)
C
               ELSE
                  VIS1P = 0.0D0
                  ADV1P = 0.0D0
               END IF
C
               VIS1 = VIS1M*XCP(8,I,J) + VIS1P*XCP(7,I,J)
               ADV1 = ADV1M*XCP(8,I,J) + ADV1P*XCP(7,I,J)
            END IF
C
            IF(ISW(4).NE.0) ADV1=0.0D0
C
            FV(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
