      SUBROUTINE FLUXWX_AIR(FU,UUA,WWA,HUA,WWX,FFA,TMUA,XC,ZCA,GVA,GXA,
     $                      INDUA,INDWA,KFA)
C======================================================================
C     Z方向の運動量保存式のX方向界面の運動量流束を計算する
C     FU: X方向格子点、Y方向セル中心、Z方向格子点で定義
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
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),WWA(MX,MY,MZA),HUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::WWX(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDWA(MX,MY,MZA),KFA(MX,MY)
C
      REAL(8)::XC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::RNU,TMU1,TMU2,TMU0,WW1,WW2,HU1
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDU1,INDU2,INB1
C
C
      CALL ZERCLR(FU,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=2,MYM
      DO 100 I=1,MXM
C
         KF1 = MIN(KFA(I,J),KFA(I+1,J))
         IF( K.GE.KF1 ) THEN
C
C ...... ±Xのどちらかの側が流速のZ方向成分計算点である
         IF( INDWA(I,J,K).GT.0 .OR. INDWA(I+1,J,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDWA(I,J,K).GT.0 ) THEN
               INB1 = I
               XC1  = 2.0D0*XC(6,I,J)
            ELSE
               INB1 = I+1
               XC1  = -2.0D0*XC(6,I+1,J)
            END IF
C
            INDU1 = INDUA(I,J,K  )
            INDU2 = INDUA(I,J,K+1)
C
C ......... 各メッシュの層厚比を計算しておく
C ......... 壁面でのFFは勾配0条件と仮定
            IF( INDU1.EQ.-2 ) THEN
               FF1 = FFA(INB1,J,K)
               HH1 = MAX(1.0D0-FF1-GVA(INB1,J,K),0.0D0)
            ELSE
               FF1 = FFA(I,J,K)*XC(7,I,J)+FFA(I+1,J,K)*XC(8,I,J)
               HH1 = MAX(1.0D0-FF1-GXA(I,J,K),0.0D0)
            END IF
C
            IF( INDU2.EQ.-2 ) THEN
               FF2 = FFA(INB1,J,K+1)
               HH2 = MAX(1.0D0-FF2-GVA(INB1,J,K+1),0.0D0)
            ELSE
               FF2 = FFA(I,J,K+1)*XC(7,I,J)+FFA(I+1,J,K+1)*XC(8,I,J)
               HH2 = MAX(1.0D0-FF2-GXA(I,J,K+1),0.0D0)
            END IF
C
            HH0 = HH1*ZCA(8,K)+HH2*ZCA(7,K)
C
C ......... (1) 境界条件なし
            IF( INDU1.GT.0 .AND. INDU2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1=TMUA(I  ,J,K)*ZCA(7,K)+TMUA(I  ,J,K+1)*ZCA(8,K)
               TMU2=TMUA(I+1,J,K)*ZCA(7,K)+TMUA(I+1,J,K+1)*ZCA(8,K)
               TMU0=TMU1*XC(7,I,J)+TMU2*XC(8,I,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*((WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
     $             +         (UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K))
C
C ............ 慣性項
               WW1 =WWA(I,J,K)*XC(7,I,J)+WWA(I+1,J,K)*XC(8,I,J)
               HU1 =HUA(I,J,K)*ZCA(8,K)+HUA(I,J,K+1)*ZCA(7,K)
               ADV1=PARAMAIR2* WW1*HU1
     $             +PARAMAIR *(WWA(I  ,J,K)*MAX(HU1,0.0D0)
     $                        +WWA(I+1,J,K)*MIN(HU1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDU1.EQ.0 .AND. INDU2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(INB1,J,K)*ZCA(7,K)+TMUA(INB1,J,K+1)*ZCA(8,K)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=HH0*RNU*(UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
C
C ............ 慣性項
               HU1 =HUA(I,J,K)*ZCA(8,K)+HUA(I,J,K+1)*ZCA(7,K)
               ADV1=WWA(INB1,J,K)*HU1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDU1.EQ.-1 .AND. INDU2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0=TMUA(INB1,J,K)*ZCA(7,K)+TMUA(INB1,J,K+1)*ZCA(8,K)
               RNU =AMUAIR/RHOAIR+TMU0
               WW1 =WWA(INB1,J,K)
               WW2 =WWA(INB1,J,K)
C
               VIS1=RNU
     $             * (HH0         *(UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
     $             +  HH1*ZCA(8,K)*(WWX(I,J,K  )-WW1)*XC1
     $             +  HH2*ZCA(7,K)*(WWX(I,J,K+1)-WW2)*XC1)
C
C ............ 慣性項
               ADV1=WWX(I,J,K  )*HUA(I,J,K  )*ZCA(8,K)
     $             +WWX(I,J,K+1)*HUA(I,J,K+1)*ZCA(7,K)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：INB1,KセルとINB1,K+1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 下側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDU1.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K)*XC(7,I,J)+TMUA(I+1,J,K)*XC(8,I,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
     $                 + (UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K))
C
                  WW1  =WWA(I,J,K)*XC(7,I,J)+WWA(I+1,J,K)*XC(8,I,J)
                  ADV1M=PARAMAIR2 *WW1*HUA(I,J,K)
     $                 +PARAMAIR *(WWA(I  ,J,K)*MAX(HUA(I,J,K),0.0D0)
     $                            +WWA(I+1,J,K)*MIN(HUA(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDU1.EQ.0 ) THEN
                  TMU0 =TMUA(INB1,J,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *(UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
C
                  ADV1M=WWA(INB1,J,K)*HUA(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU1.EQ.-1 .OR. INDU1.EQ.-2 ) THEN
                  WW1  =WWA(INB1,J,K)
cc                  WW1 = 0.5D0*WWA(INB1,J,K-1)+0.5D0*WWA(INB1,J,K)
                  IF( WWX(I,J,K).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 =TMUA(INB1,J,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=HH1*RNU
     $                 *((WWX(I,J,K)-WW1)*XC1
     $                 + (UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K))
C
                  ADV1M=WWX(I,J,K)*HUA(I,J,K)
C
               ELSE
                  VIS1M=0.0D0
                  ADV1M=0.0D0
               END IF
C
C           <<< 上側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDU2.GT.0 ) THEN
                  TMU0=TMUA(I,J,K+1)*XC(7,I,J)+TMUA(I+1,J,K+1)*XC(8,I,J)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
     $                 + (UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K))
C
                  WW1  =WWA(I,J,K)*XC(7,I,J)+WWA(I+1,J,K)*XC(8,I,J)
                  ADV1P=PARAMAIR2* WW1*HUA(I,J,K+1)
     $                 +PARAMAIR *(WWA(I  ,J,K)*MAX(HUA(I,J,K+1),0.0D0)
     $                            +WWA(I+1,J,K)*MIN(HUA(I,J,K+1),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDU2.EQ.0 ) THEN
                  TMU0 =TMUA(INB1,J,K+1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *(UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
C
                  ADV1P=WWA(INB1,J,K)*HUA(I,J,K+1)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU2.EQ.-1 .OR. INDU2.EQ.-2 ) THEN
                  WW1  =WWA(INB1,J,K)
cc                  WW1 = 0.5D0*WWA(INB1,J,K)+0.5D0*WWA(INB1,J,K+1)
                  IF( WWX(I,J,K+1).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 =TMUA(INB1,J,K+1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=HH2*RNU
     $                 *((WWX(I,J,K+1)-WW1)*XC1
     $                 + (UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K))
C
                  ADV1P=WWX(I,J,K+1)*HUA(I,J,K+1)
C
               ELSE
                  VIS1P=0.0D0
                  ADV1P=0.0D0
               END IF
C
               VIS1 = VIS1M*ZCA(8,K) + VIS1P*ZCA(7,K)
               ADV1 = ADV1M*ZCA(8,K) + ADV1P*ZCA(7,K)
cccc
ccc               if( indu1.eq.-2.or.indu2.eq.-2 ) then
ccc                  adv1=0.0d0
ccc               endif
            END IF
C
            FU(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
