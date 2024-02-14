      SUBROUTINE FLUXWY_AIR(FV,VVA,WWA,HVA,WWY,FFA,TMUA,YC,ZCA,GVA,GYA,
     $                      INDVA,INDWA,KFA)
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
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FV(MX,MY,MZA)
      REAL(8),INTENT(IN)::VVA(MX,MY,MZA),WWA(MX,MY,MZA),HVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::WWY(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GYA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA),KFA(MX,MY)
C
      REAL(8)::YC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::RNU,TMU1,TMU2,TMU0,WW1,WW2,HV1
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDV1,INDV2,JNB1
C
C
      CALL ZERCLR(FV,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=1,MYM
      DO 100 I=2,MXM
C
         KF1 = MIN(KFA(I,J),KFA(I,J+1))
         IF( K.GE.KF1 ) THEN
c
C ...... ±Yのどちらかの側が流速のZ方向成分計算点である
         IF( INDWA(I,J,K).GT.0 .OR. INDWA(I,J+1,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDWA(I,J,K).GT.0 ) THEN
               JNB1 = J
               YC1  = 2.0D0*YC(6,J)
            ELSE
               JNB1 = J+1
               YC1  = -2.0D0*YC(6,J+1)
            END IF
C
            INDV1 = INDVA(I,J,K  )
            INDV2 = INDVA(I,J,K+1)
C
C ......... 各メッシュの層厚比を計算しておく
C ......... 壁面でのFFは勾配0条件と仮定

            IF( INDV1.EQ.-2 ) THEN
               FF1 = FFA(I,JNB1,K)
               HH1 = MAX(1.0D0-FF1-GVA(I,JNB1,K),0.0D0)
            ELSE
               FF1 = FFA(I,J,K)*YC(7,J)+FFA(I,J+1,K)*YC(8,J)
               HH1 = MAX(1.0D0-FF1-GYA(I,J,K),0.0D0)
            END IF
C
            IF( INDV2.EQ.-2 ) THEN
               FF2 = FFA(I,JNB1,K+1)
               HH2 = MAX(1.0D0-FF2-GVA(I,JNB1,K+1),0.0D0)
            ELSE
               FF2 = FFA(I,J,K+1)*YC(7,J)+FFA(I,J+1,K+1)*YC(8,J)
               HH2 = MAX(1.0D0-FF2-GYA(I,J,K+1),0.0D0)
            END IF
C
            HH0 = HH1*ZCA(8,K)+HH2*ZCA(7,K)
C
C ......... (1) 境界条件なし
            IF( INDV1.GT.0 .AND. INDV2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1=TMUA(I,J  ,K)*ZCA(7,K)+TMUA(I,J  ,K+1)*ZCA(8,K)
               TMU2=TMUA(I,J+1,K)*ZCA(7,K)+TMUA(I,J+1,K+1)*ZCA(8,K)
               TMU0=TMU1*YC(7,J)+TMU2*YC(8,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*((WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
     $             +         (VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K))
C
C ............ 慣性項
               WW1 =WWA(I,J,K)*YC(7,J)+WWA(I,J+1,K)*YC(8,J)
               HV1 =HVA(I,J,K)*ZCA(8,K)+HVA(I,J,K+1)*ZCA(7,K)
               ADV1=PARAMAIR2* WW1*HV1
     $             +PARAMAIR *(WWA(I,J  ,K)*MAX(HV1,0.0D0)
     $                        +WWA(I,J+1,K)*MIN(HV1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDV1.EQ.0 .AND. INDV2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(I,JNB1,K)*ZCA(7,K)+TMUA(I,JNB1,K+1)*ZCA(8,K)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*(VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
C
C ............ 慣性項
               HV1 =HVA(I,J,K)*ZCA(8,K)+HVA(I,J,K+1)*ZCA(7,K)
               ADV1=WWA(I,JNB1,K)*HV1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDV1.EQ.-1 .AND. INDV2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(I,JNB1,K)*ZCA(7,K)+TMUA(I,JNB1,K+1)*ZCA(8,K)
               RNU =AMUAIR/RHOAIR+TMU0
               WW1 =WWA(I,JNB1,K)
               WW2 =WWA(I,JNB1,K)
C
               VIS1=RNU
     $             * (HH0         *(VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
     $             +  HH1*ZCA(8,K)*(WWY(I,J,K  )-WW1)*YC1
     $             +  HH2*ZCA(7,K)*(WWY(I,J,K+1)-WW2)*YC1)
C
C ............ 慣性項
               ADV1=WWY(I,J,K  )*HVA(I,J,K  )*ZCA(8,K)
     $             +WWY(I,J,K+1)*HVA(I,J,K+1)*ZCA(7,K)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：JNB1,KセルとJNB1,K+1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 下側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDV1.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K)*YC(7,J)+TMUA(I,J+1,K)*YC(8,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
     $                 + (VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K))
C
                  WW1  =WWA(I,J,K)*YC(7,J)+WWA(I,J+1,K)*YC(8,J)
                  ADV1M=PARAMAIR2* WW1*HVA(I,J,K)
     $                 +PARAMAIR *(WWA(I,J  ,K)*MAX(HVA(I,J,K),0.0D0)
     $                            +WWA(I,J+1,K)*MIN(HVA(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDV1.EQ.0 ) THEN
                  TMU0 =TMUA(I,JNB1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *(VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
C
                  ADV1M=WWA(I,JNB1,K)*HVA(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV1.EQ.-1 .OR. INDV1.EQ.-2 ) THEN
                  WW1  =WWA(I,JNB1,K)
                  IF( WWY(I,J,K).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 =TMUA(I,JNB1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((WWY(I,J,K)-WW1)*YC1
     $                 + (VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K))
C
                  ADV1M=WWY(I,J,K)*HVA(I,J,K)
C
               ELSE
                  VIS1M=0.0D0
                  ADV1M=0.0D0
               END IF
C
C           <<< 上側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDV2.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K+1)*YC(7,J)+TMUA(I,J+1,K+1)*YC(8,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
     $                 + (VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K))
C
                  WW1  =WWA(I,J,K)*YC(7,J)+WWA(I,J+1,K)*YC(8,J)
                  ADV1P=PARAMAIR2* WW1*HVA(I,J,K+1)
     $                 +PARAMAIR *(WWA(I,J  ,K)*MAX(HVA(I,J,K+1),0.0D0)
     $                            +WWA(I,J+1,K)*MIN(HVA(I,J,K+1),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDV2.EQ.0 ) THEN
                  TMU0 =TMUA(I,JNB1,K+1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *(VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
C
                  ADV1P=WWA(I,JNB1,K)*HVA(I,J,K+1)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV2.EQ.-1 .OR. INDV2.EQ.-2 ) THEN
                  WW1  =WWA(I,JNB1,K)
                  IF( WWY(I,J,K+1).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 =TMUA(I,JNB1,K+1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((WWY(I,J,K+1)-WW1)*YC1
     $                 + (VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K))
C
                  ADV1P=WWY(I,J,K+1)*HVA(I,J,K+1)
C
               ELSE
                  VIS1P=0.0D0
                  ADV1P=0.0D0
               END IF
C
               VIS1 = VIS1M*ZCA(8,K) + VIS1P*ZCA(7,K)
               ADV1 = ADV1M*ZCA(8,K) + ADV1P*ZCA(7,K)
            END IF
C
            FV(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
