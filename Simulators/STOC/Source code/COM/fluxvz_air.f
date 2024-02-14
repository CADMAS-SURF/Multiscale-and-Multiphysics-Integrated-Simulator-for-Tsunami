      SUBROUTINE FLUXVZ_AIR(FW,VVA,WWA,HWA,VVZ,TMUA,YC,ZCA,
     $                      GZA,INDVA,INDWA,KFA)
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
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FW(MX,MY,MZA)
      REAL(8),INTENT(IN)::VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HWA(MX,MY,MZA),VVZ(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::GZA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::ADV1,ADV1M,ADV1P,VIS1,VIS1M,VIS1P
      REAL(8)::VV1,VV2,GZ1,HW1,TMU0,TMU1,TMU2,ZC1,RNU
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,KNB1,INDW1,INDW2
C
C
      CALL ZERCLR(FW,MXY*MZA,0.0D0)
C
      DO 100 K=1,MZMA
C     DO 100 J=2,MYM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
         KF1 = MIN(KFA(I,J),KFA(I,J+1))
         IF( K.GE.KF1-1 ) THEN
C
C ...... ±Zのどちらかの側が流速のY方向成分計算点である
         IF( INDVA(I,J,K).GT.0 .OR. INDVA(I,J,K+1).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDVA(I,J,K).GT.0 ) THEN
               KNB1 = K
               ZC1  = 2.0D0*ZCA(6,K)
            ELSE
               KNB1 = K+1
               ZC1  = -2.0D0*ZCA(6,K+1)
            END IF
C
            INDW1 = INDWA(I,J  ,K)
            INDW2 = INDWA(I,J+1,K)
C
C ......... (1) 境界条件なし
            IF( INDW1.GT.0 .AND. INDW2.GT.0 ) THEN
C
C ............ 粘性項
               GZ1 =1.0D0-(GZA(I,J,K)*YC(8,J)+GZA(I,J+1,K)*YC(7,J))
               TMU1=TMUA(I,J,K  )*YC(7,J)+TMUA(I,J+1,K  )*YC(8,J)
               TMU2=TMUA(I,J,K+1)*YC(7,J)+TMUA(I,J+1,K+1)*YC(8,J)
               TMU0=TMU1*ZCA(7,K)+TMU2*ZCA(8,K)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=GZ1*RNU*((VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
     $             +         (WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J))
C
C ............ 慣性項
               VV1 =VVA(I,J,K)*ZCA(7,K)+VVA(I,J,K+1)*ZCA(8,K)
               HW1 =HWA(I,J,K)*YC(8,J)+HWA(I,J+1,K)*YC(7,J)
               ADV1=PARAMAIR2* VV1*HW1
     $             +PARAMAIR *(VVA(I,J,K  )*MAX(HW1,0.0D0)
     $                        +VVA(I,J,K+1)*MIN(HW1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDW1.EQ.0 .AND. INDW2.EQ.0 ) THEN
C
C ............ 粘性項
               GZ1 =1.0D0-(GZA(I,J,K)*YC(8,J)+GZA(I,J+1,K)*YC(7,J))
               TMU0=TMUA(I,J,KNB1)*YC(7,J)+TMUA(I,J+1,KNB1)*YC(8,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=GZ1*RNU*(WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
C
C ............ 慣性項
               HW1 =HWA(I,J,K)*YC(8,J)+HWA(I,J+1,K)*YC(7,J)
               ADV1=VVA(I,J,KNB1)*HW1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDW1.EQ.-1 .AND. INDW2.EQ.-1 ) THEN
C
C ............ 粘性項
               GZ1 =1.0D0-(GZA(I,J,K)*YC(8,J)+GZA(I,J+1,K)*YC(7,J))
               TMU0=TMUA(I,J,KNB1)*YC(7,J)+TMUA(I,J+1,KNB1)*YC(8,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VV1 =VVA(I,J,KNB1)
               VV2 =VVA(I,J,KNB1)
C
               VIS1=RNU
     $           *(GZ1*(WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
     $           +(1.0D0-GZA(I,J  ,K))*YC(8,J)*(VVZ(I,J  ,K)-VV1)*ZC1
     $           +(1.0D0-GZA(I,J+1,K))*YC(7,J)*(VVZ(I,J+1,K)-VV2)*ZC1)
C
C ............ 慣性項
               ADV1=VVZ(I,J  ,K)*HWA(I,J  ,K)*YC(8,J)
     $             +VVZ(I,J+1,K)*HWA(I,J+1,K)*YC(7,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：J,KNB1セルとJ+1,KNB1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDW1.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K)*ZCA(7,K)+TMUA(I,J,K+1)*ZCA(8,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=(1.0D0-GZA(I,J,K))*RNU
     $                 *((VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
     $                 + (WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J))
C
                  VV1  =VVA(I,J,K)*ZCA(7,K)+VVA(I,J,K+1)*ZCA(8,K)
                  ADV1M=PARAMAIR2* VV1*HWA(I,J,K)
     $                 +PARAMAIR *(VVA(I,J,K  )*MAX(HWA(I,J,K),0.0D0)
     $                            +VVA(I,J,K+1)*MIN(HWA(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDW1.EQ.0 ) THEN
                  TMU0 =TMUA(I,J,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=(1.0D0-GZA(I,J,K))*RNU
     $                 *(WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
C
                  ADV1M=VVA(I,J,KNB1)*HWA(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW1.EQ.-1 .OR. INDW1.EQ.-2 ) THEN
                  VV1  =VVA(I,J,KNB1)
                  IF( VVZ(I,J,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 =TMUA(I,J,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=(1.0D0-GZA(I,J,K))*RNU
     $                 *((VVZ(I,J,K)-VV1)*ZC1
     $                 + (WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J))
C
                  ADV1M=VVZ(I,J,K)*HWA(I,J,K)
C
               ELSE
                  VIS1M=0.0D0
                  ADV1M=0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDW2.GT.0 ) THEN
                  TMU0 =TMUA(I,J+1,K)*ZCA(7,K)+TMUA(I,J+1,K+1)*ZCA(8,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=(1.0D0-GZA(I,J+1,K))*RNU
     $                 *((VVA(I,J,K+1)-VVA(I,J,K))*ZCA(5,K)
     $                 + (WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J))
C
                  VV1  =VVA(I,J,K)*ZCA(7,K)+VVA(I,J,K+1)*ZCA(8,K)
                  ADV1P=PARAMAIR2* VV1*HWA(I,J+1,K)
     $                 +PARAMAIR *(VVA(I,J,K  )*MAX(HWA(I,J+1,K),0.0D0)
     $                            +VVA(I,J,K+1)*MIN(HWA(I,J+1,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDW2.EQ.0 ) THEN
                  TMU0 =TMUA(I,J+1,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=(1.0D0-GZA(I,J+1,K))*RNU
     $                 *(WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J)
C
                  ADV1P=VVA(I,J,KNB1)*HWA(I,J+1,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW2.EQ.-1 .OR. INDW2.EQ.-2 ) THEN
                  VV1  =VVA(I,J,KNB1)
                  IF( VVZ(I,J+1,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 =TMUA(I,J+1,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=(1.0D0-GZA(I,J+1,K))*RNU
     $                 *((VVZ(I,J+1,K)-VV1)*ZC1
     $                 + (WWA(I,J+1,K)-WWA(I,J,K))*YC(5,J))
C
                  ADV1P=VVZ(I,J+1,K)*HWA(I,J+1,K)
C
               ELSE
                  VIS1P=0.0D0
                  ADV1P=0.0D0
               END IF
C
               VIS1 = VIS1M*YC(8,J) + VIS1P*YC(7,J)
               ADV1 = ADV1M*YC(8,J) + ADV1P*YC(7,J)
            END IF
C
            FW(I,J,K) = VIS1 - ADV1
         END IF
         END IF
C
  100 CONTINUE
C
      RETURN
      END
