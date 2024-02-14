      SUBROUTINE FLUXVX_AIR(FU,UUA,VVA,HUA,VVX,FFA,TMUA,XC,YC,XCP,
     $                      GVA,GXA,INDUA,INDVA,KFA)
C======================================================================
C     Y方向の運動量保存式のX方向界面の運動量流束を計算する
C     FU: X方向格子点、Y方向格子点、Z方向セル中心で定義
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
      REAL(8),INTENT(OUT)::FU(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),HUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::VVX(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),XCP(8,MX,MY)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA),KFA(MX,MY)
C
      REAL(8)::XC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::VV1,VV2,HU1,RNU,TMU0,TMU1,TMU2
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDU1,INDU2,INB1
C
C
      CALL ZERCLR(FU,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
C     DO 100 J=2,MYM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 J=2,MYM
      DO 100 I=1,MXM
C
         KF1 = MIN(KFA(I,J),KFA(I+1,J),KFA(I,J+1),KFA(I+1,J+1))
         IF( K.GE.KF1 ) THEN
C
C ...... ±Xのどちらかの側が流速のY方向成分計算点である
         IF( INDVA(I,J,K).GT.0 .OR. INDVA(I+1,J,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDVA(I,J,K).GT.0 ) THEN
               INB1 = I
               XC1  = 2.0D0*XCP(6,I,J)
            ELSE
               INB1 = I+1
               XC1  = -2.0D0*XCP(6,I+1,J)
            END IF
C
            INDU1 = INDUA(I,J  ,K)
            INDU2 = INDUA(I,J+1,K)
C
C ......... 各メッシュの層厚比を計算しておく
C ......... 壁面でのFFは勾配0条件と仮定
            FF1 = MAX(FFA(I,J  ,K),FFA(I+1,J  ,K))
            FF2 = MAX(FFA(I,J+1,K),FFA(I+1,J+1,K))
            HH1 = MAX(1.0D0-FF1-GXA(I,J  ,K),0.0D0)
            HH2 = MAX(1.0D0-FF2-GXA(I,J+1,K),0.0D0)
            IF( INDU1.EQ.-2 ) HH1 = MAX(1.0D0-FF1-GVA(INB1,J  ,K),0.0D0)
            IF( INDU2.EQ.-2 ) HH2 = MAX(1.0D0-FF2-GVA(INB1,J+1,K),0.0D0)
            HH0 = HH1*YC(8,J)+HH2*YC(7,J)
C
C ......... (1) 境界条件なし
            IF( INDU1.GT.0 .AND. INDU2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1=TMUA(I  ,J,K)*YC(7,J)+TMUA(I  ,J+1,K)*YC(8,J)
               TMU2=TMUA(I+1,J,K)*YC(7,J)+TMUA(I+1,J+1,K)*YC(8,J)
               TMU0=TMU1*XCP(7,I,J)+TMU2*XCP(8,I,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*((VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
     $             +         (UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J))
C
C ............ 慣性項
               VV1 =VVA(I,J,K)*XCP(7,I,J)+VVA(I+1,J,K)*XCP(8,I,J)
               HU1 =HUA(I,J,K)*YC(8,J)+HUA(I,J+1,K)*YC(7,J)
               ADV1=PARAMAIR2* VV1*HU1
     $             +PARAMAIR *(VVA(I  ,J,K)*MAX(HU1,0.0D0)
     $                        +VVA(I+1,J,K)*MIN(HU1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDU1.EQ.0 .AND. INDU2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(INB1,J,K)*YC(7,J)+TMUA(INB1,J+1,K)*YC(8,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*(UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
C
C ............ 慣性項
               HU1 =HUA(I,J,K)*YC(8,J)+HUA(I,J+1,K)*YC(7,J)
               ADV1=VVA(INB1,J,K)*HU1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDU1.EQ.-1 .AND. INDU2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(INB1,J,K)*YC(7,J)+TMUA(INB1,J+1,K)*YC(8,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VV1 =VVA(INB1,J,K)
               VV2 =VVA(INB1,J,K)
C
               VIS1=RNU
     $             *(HH0        *(UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
     $             + HH1*YC(8,J)*(VVX(I,J  ,K)-VV1)*XC1
     $             + HH2*YC(7,J)*(VVX(I,J+1,K)-VV2)*XC1)
C
C ............ 慣性項
               ADV1=VVX(I,J  ,K)*HUA(I,J  ,K)*YC(8,J)
     $             +VVX(I,J+1,K)*HUA(I,J+1,K)*YC(7,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：INB1,JセルとINB1,J+1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDU1.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K)*XC(7,I,J)+TMUA(I+1,J,K)*XC(8,I,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
     $                 + (UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J))
C
                  VV1  =VVA(I,J,K)*XCP(7,I,J)+VVA(I+1,J,K)*XCP(8,I,J)
                  ADV1M=PARAMAIR2 *VV1*HUA(I,J,K)
     $                 +PARAMAIR *(VVA(I  ,J,K)*MAX(HUA(I,J,K),0.0D0)
     $                            +VVA(I+1,J,K)*MIN(HUA(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDU1.EQ.0 ) THEN
                  TMU0 =TMUA(INB1,J,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *(UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
C
                  ADV1M=VVA(INB1,J,K)*HUA(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU1.EQ.-1 .OR. INDU1.EQ.-2 ) THEN
                  VV1  =VVA(INB1,J,K)
                  IF( VVX(I,J,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 =TMUA(INB1,J,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((VVX(I,J,K)-VV1)*XC1
     $                 + (UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J))
C
                  ADV1M=VVX(I,J,K)*HUA(I,J,K)
C
               ELSE
                  VIS1M=0.0D0
                  ADV1M=0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDU2.GT.0 ) THEN
                  TMU0 =TMUA(I  ,J+1,K)*XC(7,I,J+1)
     $                 +TMUA(I+1,J+1,K)*XC(8,I,J+1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((VVA(I+1,J,K)-VVA(I,J,K))*XCP(5,I,J)
     $                 + (UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J))
C
                  VV1  =VVA(I,J,K)*XCP(7,I,J)+VVA(I+1,J,K)*XCP(8,I,J)
                  ADV1P=PARAMAIR2* VV1*HUA(I,J+1,K)
     $                 +PARAMAIR *(VVA(I  ,J,K)*MAX(HUA(I,J+1,K),0.0D0)
     $                            +VVA(I+1,J,K)*MIN(HUA(I,J+1,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDU2.EQ.0 ) THEN
                  TMU0 =TMUA(INB1,J+1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *(UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J)
C
                  ADV1P=VVA(INB1,J,K)*HUA(I,J+1,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU2.EQ.-1 .OR. INDU2.EQ.-2 ) THEN
                  VV1  =VVA(INB1,J,K)
                  IF( VVX(I,J+1,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 =TMUA(INB1,J+1,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((VVX(I,J+1,K)-VV1)*XC1
     $                 + (UUA(I,J+1,K)-UUA(I,J,K))*YC(5,J))
C
                  ADV1P=VVX(I,J+1,K)*HUA(I,J+1,K)
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
            FU(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
