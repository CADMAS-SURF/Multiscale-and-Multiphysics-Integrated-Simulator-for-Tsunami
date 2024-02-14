      SUBROUTINE FLUXWY(FV,VV,WW,HV,WWY,FF,TMU,YC,ZC,GV,GY,GV0,GY0,
     $                  INDV,INDW,KF)
C======================================================================
C     Z方向の運動量保存式のY方向界面の運動量流束を計算する
C     FV: X方向セル中心、Y方向格子点、Z方向格子点で定義
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
      REAL(8),INTENT(INOUT)::VV(MX,MY,MZ),WW(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WWY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::YC(8,MY),ZC(8,MZ),GV(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GY0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ),KF(MX,MY)
C
      REAL(8)::YC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::TMU1,TMU2,TMU0,WW1,WW2,HV1
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDV1,INDV2,JNB1
C
C
      CALL ZERCLR(FV,MXYZ,0.0D0)
C
      DO 100 K=2,MZM-1
      DO 100 J=1,MYM
      DO 100 I=2,MXM
C
         KF1 = MAX(KF(I,J),KF(I,J+1))-1
         IF( K.LE.KF1 ) THEN
c
C ...... ±Yのどちらかの側が流速のZ方向成分計算点である
         IF( INDW(I,J,K).GT.0 .OR. INDW(I,J+1,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDW(I,J,K).GT.0 ) THEN
               JNB1 = J
               YC1  = 2.0D0*YC(6,J)
            ELSE
               JNB1 = J+1
               YC1  = -2.0D0*YC(6,J+1)
            END IF
C
            INDV1 = INDV(I,J,K  )
            INDV2 = INDV(I,J,K+1)
C
C ......... 各メッシュの層厚比を計算しておく
C ......... 壁面でのFFは勾配0条件と仮定

            IF( INDV1.EQ.-2 ) THEN
               FF1 = FF(I,JNB1,K)
               HH1 = MAX(FF1-1.0D0+GV0(I,JNB1,K),0.0D0)
     $            *GV(I,JNB1,K)/GV0(I,JNB1,K)
            ELSE
               FF1 = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
               HH1 = MAX(FF1-1.0D0+GY0(I,J,K),0.0D0)
     $            *GY(I,J,K)/GY0(I,J,K)
            END IF
C
            IF( INDV2.EQ.-2 ) THEN
               FF2 = FF(I,JNB1,K+1)
               HH2 = MAX(FF2-1.0D0+GV0(I,JNB1,K+1),0.0D0)
     $            *GV(I,JNB1,K+1)/GV0(I,JNB1,K+1)
            ELSE
               FF2 = FF(I,J,K+1)*YC(7,J)+FF(I,J+1,K+1)*YC(8,J)
               HH2 = MAX(FF2-1.0D0+GY0(I,J,K+1),0.0D0)
     $            *GY(I,J,K+1)/GY0(I,J,K+1)
            END IF
C
            HH0 = HH1*ZC(8,K)+HH2*ZC(7,K)
C
C ......... (1) 境界条件なし
            IF( INDV1.GT.0 .AND. INDV2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1 = TMU(I,J  ,K)*ZC(7,K)+TMU(I,J  ,K+1)*ZC(8,K)
               TMU2 = TMU(I,J+1,K)*ZC(7,K)+TMU(I,J+1,K+1)*ZC(8,K)
               TMU0 = TMU1*YC(7,J)+TMU2*YC(8,J)
               VIS1 = HH0*(ANUH+TMU0)*((WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
     $              +                  (VV(I,J,K+1)-VV(I,J,K))*ZC(5,K))
C
C ............ 慣性項
               WW1 = WW(I,J,K)*YC(7,J)+WW(I,J+1,K)*YC(8,J)
               HV1 = HV(I,J,K)*ZC(8,K)+HV(I,J,K+1)*ZC(7,K)
               ADV1 = PARAMV2* WW1*HV1
     $              + PARAMV *(WW(I,J  ,K)*MAX(HV1,0.0D0)
     $                        +WW(I,J+1,K)*MIN(HV1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDV1.EQ.0 .AND. INDV2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(I,JNB1,K)*ZC(7,K)+TMU(I,JNB1,K+1)*ZC(8,K)
               VIS1 = HH0*(ANUH+TMU0)*(VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
C
C ............ 慣性項
               HV1 = HV(I,J,K)*ZC(8,K)+HV(I,J,K+1)*ZC(7,K)
               ADV1 = WW(I,JNB1,K)*HV1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDV1.EQ.-1 .AND. INDV2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(I,JNB1,K)*ZC(7,K)+TMU(I,JNB1,K+1)*ZC(8,K)
               WW1  = WW(I,JNB1,K)
               WW2  = WW(I,JNB1,K)
cdist          WW1  = 0.25D0*WW(I,JNB1,K-1)+0.75D0*WW(I,JNB1,K  )
cdist          WW2  = 0.75D0*WW(I,JNB1,K  )+0.25D0*WW(I,JNB1,K+1)
C
               VIS1 = (ANUH+TMU0)
     $              * (HH0        *(VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
     $              +  HH1*ZC(8,K)*(WWY(I,J,K  )-WW1)*YC1
     $              +  HH2*ZC(7,K)*(WWY(I,J,K+1)-WW2)*YC1)
C
C ............ 慣性項
               ADV1 = WWY(I,J,K  )*HV(I,J,K  )*ZC(8,K)
     $              + WWY(I,J,K+1)*HV(I,J,K+1)*ZC(7,K)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：JNB1,KセルとJNB1,K+1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 下側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDV1.GT.0 ) THEN
                  TMU0 = TMU(I,J,K)*YC(7,J)+TMU(I,J+1,K)*YC(8,J)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
     $                  + (VV(I,J,K+1)-VV(I,J,K))*ZC(5,K))
C
                  WW1 = WW(I,J,K)*YC(7,J)+WW(I,J+1,K)*YC(8,J)
                  ADV1M = PARAMV2* WW1*HV(I,J,K)
     $                  + PARAMV *(WW(I,J  ,K)*MAX(HV(I,J,K),0.0D0)
     $                            +WW(I,J+1,K)*MIN(HV(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDV1.EQ.0 ) THEN
                  TMU0 = TMU(I,JNB1,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *(VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
C
                  ADV1M = WW(I,JNB1,K)*HV(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV1.EQ.-1 .OR. INDV1.EQ.-2 ) THEN
                  WW1 = WW(I,JNB1,K)
cdist             WW1 = 0.25D0*WW(I,JNB1,K-1)+0.75D0*WW(I,JNB1,K)
                  IF( WWY(I,J,K).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 = TMU(I,JNB1,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((WWY(I,J,K)-WW1)*YC1
     $                  + (VV(I,J,K+1)-VV(I,J,K))*ZC(5,K))
C
                  ADV1M = WWY(I,J,K)*HV(I,J,K)
C
               ELSE
                  VIS1M = 0.0D0
                  ADV1M = 0.0D0
               END IF
C
C           <<< 上側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDV2.GT.0 ) THEN
                  TMU0 = TMU(I,J,K+1)*YC(7,J)+TMU(I,J+1,K+1)*YC(8,J)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((WW(I,J+1,K)-WW(I,J,K))*YC(5,J)
     $                  + (VV(I,J,K+1)-VV(I,J,K))*ZC(5,K))
C
                  WW1 = WW(I,J,K)*YC(7,J)+WW(I,J+1,K)*YC(8,J)
                  ADV1P = PARAMV2* WW1*HV(I,J,K+1)
     $                  + PARAMV *(WW(I,J  ,K)*MAX(HV(I,J,K+1),0.0D0)
     $                            +WW(I,J+1,K)*MIN(HV(I,J,K+1),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDV2.EQ.0 ) THEN
                  TMU0 = TMU(I,JNB1,K+1)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *(VV(I,J,K+1)-VV(I,J,K))*ZC(5,K)
C
                  ADV1P = WW(I,JNB1,K)*HV(I,J,K+1)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV2.EQ.-1 .OR. INDV2.EQ.-2 ) THEN
                  WW1 = WW(I,JNB1,K)
cdist             WW1 = 0.75D0*WW(I,JNB1,K)+0.25D0*WW(I,JNB1,K+1)
                  IF( WWY(I,J,K+1).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 = TMU(I,JNB1,K+1)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((WWY(I,J,K+1)-WW1)*YC1
     $                  + (VV(I,J,K+1)-VV(I,J,K))*ZC(5,K))
C
                  ADV1P = WWY(I,J,K+1)*HV(I,J,K+1)
C
               ELSE
                  VIS1P = 0.0D0
                  ADV1P = 0.0D0
               END IF
C
               VIS1 = VIS1M*ZC(8,K) + VIS1P*ZC(7,K)
               ADV1 = ADV1M*ZC(8,K) + ADV1P*ZC(7,K)
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
