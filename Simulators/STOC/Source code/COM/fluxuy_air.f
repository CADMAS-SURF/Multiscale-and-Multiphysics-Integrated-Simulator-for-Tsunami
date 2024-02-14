      SUBROUTINE FLUXUY_AIR(FV,UUA,VVA,HVA,UUY,FFA,TMUA,XC,YC,XCP,
     $                      GVA,GYA,INDUA,INDVA,KFA)
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
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FV(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),HVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUY(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),XCP(8,MX,MY)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GYA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA),KFA(MX,MY)
C
      REAL(8)::YC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::UU1,UU2,HV1,RNU,TMU0,TMU1,TMU2
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDV1,INDV2,JNB1
C
C
      CALL ZERCLR(FV,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=1,MYM
C     DO 100 I=2,MXM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 I=2,MXM
C
         KF1 = MIN(KFA(I,J),KFA(I+1,J),KFA(I,J+1),KFA(I+1,J+1))
         IF( K.GE.KF1 ) THEN
C
C ...... ±Yのどちらかの側が流速のX方向成分計算点である
         IF( INDUA(I,J,K).GT.0 .OR. INDUA(I,J+1,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDUA(I,J,K).GT.0 ) THEN
               JNB1 = J
               YC1  = 2.0D0*YC(6,J)
            ELSE
               JNB1 = J+1
               YC1  = -2.0D0*YC(6,J+1)
            END IF
C
            INDV1 = INDVA(I  ,J,K)
            INDV2 = INDVA(I+1,J,K)
C
C ......... 各メッシュの層厚比を計算しておく
C ......... 壁面でのFFは勾配0条件と仮定
            FF1 = MAX(FFA(I  ,J,K),FFA(I  ,J+1,K))
            FF2 = MAX(FFA(I+1,J,K),FFA(I+1,J+1,K))
            HH1 = MAX(1.0D0-FF1-GYA(I  ,J,K),0.0D0)
            HH2 = MAX(1.0D0-FF2-GYA(I+1,J,K),0.0D0)
            IF( INDV1.EQ.-2 ) HH1 = MAX(1.0D0-FF1-GVA(I  ,JNB1,K),0.0D0)
            IF( INDV2.EQ.-2 ) HH2 = MAX(1.0D0-FF2-GVA(I+1,JNB1,K),0.0D0)
            HH0 = HH1*XCP(8,I,J)+HH2*XCP(7,I,J)
C
C ......... (1) 境界条件なし
            IF( INDV1.GT.0 .AND. INDV2.GT.0 ) THEN
C
C ............ 粘性項
              TMU1=TMUA(I,J  ,K)*XC(7,I,J  )+TMUA(I+1,J  ,K)*XC(8,I,J  )
              TMU2=TMUA(I,J+1,K)*XC(7,I,J+1)+TMUA(I+1,J+1,K)*XC(8,I,J+1)
               TMU0=TMU1*YC(7,J)+TMU2*YC(8,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*((UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
     $             +         (VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J))
C
C ............ 慣性項
               UU1 =UUA(I,J,K)*YC(7,J)+UUA(I,J+1,K)*YC(8,J)
               HV1 =HVA(I,J,K)*XCP(8,I,J)+HVA(I+1,J,K)*XCP(7,I,J)
               ADV1=PARAMAIR2* UU1*HV1
     $             +PARAMAIR *(UUA(I,J  ,K)*MAX(HV1,0.0D0)
     $                        +UUA(I,J+1,K)*MIN(HV1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDV1.EQ.0 .AND. INDV2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(I  ,JNB1,K)*XC(7,I,JNB1)
     $             +TMUA(I+1,JNB1,K)*XC(8,I,JNB1)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*(VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
C
C ............ 慣性項
               HV1 =HVA(I,J,K)*XCP(8,I,J)+HVA(I+1,J,K)*XCP(7,I,J)
               ADV1=UUA(I,JNB1,K)*HV1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDV1.EQ.-1 .AND. INDV2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(I  ,JNB1,K)*XC(7,I,JNB1)
     $             +TMUA(I+1,JNB1,K)*XC(8,I,JNB1)
               RNU =AMUAIR/RHOAIR+TMU0
               UU1 =UUA(I,JNB1,K)
               UU2 =UUA(I,JNB1,K)
C
               VIS1=RNU
     $             *(HH0           *(VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
     $             + HH1*XCP(8,I,J)*(UUY(I  ,J,K)-UU1)*YC1
     $             + HH2*XCP(7,I,J)*(UUY(I+1,J,K)-UU2)*YC1)
C
C ............ 慣性項
               ADV1=UUY(I  ,J,K)*HVA(I  ,J,K)*XCP(8,I,J)
     $             +UUY(I+1,J,K)*HVA(I+1,J,K)*XCP(7,I,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：I,JNB1セルとI+1,JNB1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDV1.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K)*YC(7,J)+TMUA(I,J+1,K)*YC(8,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
     $                 + (VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J))
C
                  UU1  =UUA(I,J,K)*YC(7,J)+UUA(I,J+1,K)*YC(8,J)
                  ADV1M=PARAMAIR2* UU1*HVA(I,J,K)
     $                 +PARAMAIR *(UUA(I,J  ,K)*MAX(HVA(I,J,K),0.0D0)
     $                            +UUA(I,J+1,K)*MIN(HVA(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDV1.EQ.0 ) THEN
                  TMU0 =TMUA(I,JNB1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *(VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
C
                  ADV1M=UUA(I,JNB1,K)*HVA(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV1.EQ.-1 .OR. INDV1.EQ.-2 ) THEN
                  UU1  =UUA(I,JNB1,K)
                  IF( UUY(I,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 =TMUA(I,JNB1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((UUY(I,J,K)-UU1)*YC1
     $                 + (VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J))
C
                  ADV1M=UUY(I,J,K)*HVA(I,J,K)
C
               ELSE
                  VIS1M=0.0D0
                  ADV1M=0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDV2.GT.0 ) THEN
                  TMU0 =TMUA(I+1,J,K)*YC(7,J)+TMUA(I+1,J+1,K)*YC(8,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
     $                 + (VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J))
C
                  UU1  =UUA(I,J,K)*YC(7,J)+UUA(I,J+1,K)*YC(8,J)
                  ADV1P=PARAMAIR2* UU1*HVA(I+1,J,K)
     $                 +PARAMAIR *(UUA(I,J  ,K)*MAX(HVA(I+1,J,K),0.0D0)
     $                            +UUA(I,J+1,K)*MIN(HVA(I+1,J,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDV2.EQ.0 ) THEN
                  TMU0 =TMUA(I+1,JNB1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *(VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
C
                  ADV1P=UUA(I,JNB1,K)*HVA(I+1,J,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDV2.EQ.-1 .OR. INDV2.EQ.-2 ) THEN
                  UU1  =UUA(I,JNB1,K)
                  IF( UUY(I+1,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 =TMUA(I+1,JNB1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((UUY(I+1,J,K)-UU1)*YC1
     $                 + (VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J))
C
                  ADV1P=UUY(I+1,J,K)*HVA(I+1,J,K)
C
               ELSE
                  VIS1P=0.0D0
                  ADV1P=0.0D0
               END IF
C
               VIS1 = VIS1M*XCP(8,I,J) + VIS1P*XCP(7,I,J)
               ADV1 = ADV1M*XCP(8,I,J) + ADV1P*XCP(7,I,J)
            END IF
C
            FV(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
