      SUBROUTINE FLUXUZ_AIR(FW,UUA,WWA,HWA,UUZ,TMUA,XC,ZCA,
     $                      GZA,INDUA,INDWA,KFA)
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
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FW(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HWA(MX,MY,MZA),UUZ(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::GZA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::ADV1,ADV1M,ADV1P,VIS1,VIS1M,VIS1P
      REAL(8)::UU1,UU2,GZ1,HW1,TMU0,TMU1,TMU2,ZC1,RNU
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,KNB1,INDW1,INDW2
C
C
      CALL ZERCLR(FW,MXY*MZA,0.0D0)
C
      DO 100 K=1,MZMA
      DO 100 J=2,MYM
C     DO 100 I=2,MXM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 I=2,MXM
C
         KF1 = MIN(KFA(I,J),KFA(I+1,J))
         IF( K.GE.KF1-1 ) THEN
C
C ...... ±Zのどちらかの側が流速のX方向成分計算点である
         IF( INDUA(I,J,K).GT.0 .OR. INDUA(I,J,K+1).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDUA(I,J,K).GT.0 ) THEN
               KNB1 = K
               ZC1  = 2.0D0*ZCA(6,K)
            ELSE
               KNB1 = K+1
               ZC1  = -2.0D0*ZCA(6,K+1)
            END IF
C
            INDW1 = INDWA(I  ,J,K)
            INDW2 = INDWA(I+1,J,K)
C
C ......... (1) 境界条件なし
            IF( INDW1.GT.0 .AND. INDW2.GT.0 ) THEN
C
C ............ 粘性項
               GZ1 =1.0D0-(GZA(I,J,K)*XC(8,I,J)+GZA(I+1,J,K)*XC(7,I,J))
               TMU1=TMUA(I,J,K  )*XC(7,I,J)+TMUA(I+1,J,K  )*XC(8,I,J)
               TMU2=TMUA(I,J,K+1)*XC(7,I,J)+TMUA(I+1,J,K+1)*XC(8,I,J)
               TMU0=TMU1*ZCA(7,K)+TMU2*ZCA(8,K)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=GZ1*RNU*((UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
     $             +         (WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J))
C
C ............ 慣性項
               UU1 =UUA(I,J,K)*ZCA(7,K)+UUA(I,J,K+1)*ZCA(8,K)
               HW1 =HWA(I,J,K)*XC(8,I,J)+HWA(I+1,J,K)*XC(7,I,J)
               ADV1=PARAMAIR2* UU1*HW1
     $             +PARAMAIR *(UUA(I,J,K  )*MAX(HW1,0.0D0)
     $                        +UUA(I,J,K+1)*MIN(HW1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDW1.EQ.0 .AND. INDW2.EQ.0 ) THEN
C
C ............ 粘性項
               GZ1 =1.0D0-(GZA(I,J,K)*XC(8,I,J)+GZA(I+1,J,K)*XC(7,I,J))
               TMU0=TMUA(I,J,KNB1)*XC(7,I,J)+TMUA(I+1,J,KNB1)*XC(8,I,J)
               RNU =AMUAIR/RHOAIR+TMU0
               VIS1=GZ1*RNU*(WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
C
C ............ 慣性項
               HW1 =HWA(I,J,K)*XC(8,I,J)+HWA(I+1,J,K)*XC(7,I,J)
               ADV1=UUA(I,J,KNB1)*HW1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDW1.EQ.-1 .AND. INDW2.EQ.-1 ) THEN
C
C ............ 粘性項
               GZ1 =1.0D0-(GZA(I,J,K)*XC(8,I,J)+GZA(I+1,J,K)*XC(7,I,J))
               TMU0=TMUA(I,J,KNB1)*XC(7,I,J)+TMUA(I+1,J,KNB1)*XC(8,I,J)
               RNU =AMUAIR/RHOAIR+TMU0
               UU1 =UUA(I,J,KNB1)
               UU2 =UUA(I,J,KNB1)
C
               VIS1=RNU
     $           *(GZ1*(WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
     $           +(1.0D0-GZA(I  ,J,K))*XC(8,I,J)*(UUZ(I  ,J,K)-UU1)*ZC1
     $           +(1.0D0-GZA(I+1,J,K))*XC(7,I,J)*(UUZ(I+1,J,K)-UU2)*ZC1)
C
C ............ 慣性項
               ADV1=UUZ(I  ,J,K)*HWA(I  ,J,K)*XC(8,I,J)
     $             +UUZ(I+1,J,K)*HWA(I+1,J,K)*XC(7,I,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：I,KNB1セルとI+1,KNB1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDW1.GT.0 ) THEN
                  TMU0 =TMUA(I,J,K)*ZCA(7,K)+TMUA(I,J,K+1)*ZCA(8,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=(1.0D0-GZA(I,J,K))*RNU
     $                 *((UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
     $                 + (WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J))
C
                  UU1  =UUA(I,J,K)*ZCA(7,K)+UUA(I,J,K+1)*ZCA(8,K)
                  ADV1M=PARAMAIR2* UU1*HWA(I,J,K)
     $                 +PARAMAIR *(UUA(I,J,K  )*MAX(HWA(I,J,K),0.0D0)
     $                            +UUA(I,J,K+1)*MIN(HWA(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDW1.EQ.0 ) THEN
                  TMU0 =TMUA(I,J,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=(1.0D0-GZA(I,J,K))*RNU
     $                 *(WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
C
                  ADV1M=UUA(I,J,KNB1)*HWA(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW1.EQ.-1 .OR. INDW1.EQ.-2 ) THEN
                  UU1  =UUA(I,J,KNB1)
cc                  UU1 = 0.5D0*UUA(I-1,J,KNB1)+0.5D0*UUA(I,J,KNB1)
                  IF( UUZ(I,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 =TMUA(I,J,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1M=(1.0D0-GZA(I,J,K))*RNU
     $                 *((UUZ(I,J,K)-UU1)*ZC1
     $                 + (WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J))
C
                  ADV1M=UUZ(I,J,K)*HWA(I,J,K)
C
               ELSE
                  VIS1M=0.0D0
                  ADV1M=0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDW2.GT.0 ) THEN
                  TMU0 =TMUA(I+1,J,K)*ZCA(7,K)+TMUA(I+1,J,K+1)*ZCA(8,K)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=(1.0D0-GZA(I+1,J,K))*RNU
     $                 *((UUA(I,J,K+1)-UUA(I,J,K))*ZCA(5,K)
     $                 + (WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J))
C
                  UU1  =UUA(I,J,K)*ZCA(7,K)+UUA(I,J,K+1)*ZCA(8,K)
                  ADV1P=PARAMAIR2* UU1*HWA(I+1,J,K)
     $                 +PARAMAIR *(UUA(I,J,K  )*MAX(HWA(I+1,J,K),0.0D0)
     $                            +UUA(I,J,K+1)*MIN(HWA(I+1,J,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDW2.EQ.0 ) THEN
                  TMU0 =TMUA(I+1,J,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=(1.0D0-GZA(I+1,J,K))*RNU
     $                 *(WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J)
C
                  ADV1P=UUA(I,J,KNB1)*HWA(I+1,J,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDW2.EQ.-1 .OR. INDW2.EQ.-2 ) THEN
                  UU1  =UUA(I,J,KNB1)
cc                  UU1 = 0.5D0*UUA(I,J,KNB1)+0.5D0*UUA(I+1,J,KNB1)
                  IF( UUZ(I+1,J,K).EQ.VSLIP ) UU1 = VSLIP
                  TMU0 =TMUA(I+1,J,KNB1)
                  RNU  =AMUAIR/RHOAIR+TMU0
                  VIS1P=(1.0D0-GZA(I+1,J,K))*RNU
     $                 *((UUZ(I+1,J,K)-UU1)*ZC1
     $                 + (WWA(I+1,J,K)-WWA(I,J,K))*XC(5,I,J))
C
                  ADV1P=UUZ(I+1,J,K)*HWA(I+1,J,K)
C
               ELSE
                  VIS1P=0.0D0
                  ADV1P=0.0D0
               END IF
C
               VIS1 = VIS1M*XC(8,I,J) + VIS1P*XC(7,I,J)
               ADV1 = ADV1M*XC(8,I,J) + ADV1P*XC(7,I,J)
cccc
ccc               if( indw1.eq.-2.or.indw2.eq.-2 ) then
ccc                  adv1=0.0d0
ccc               endif
            END IF
C
            FW(I,J,K) = VIS1 - ADV1
c            if(i.eq.3.and.k.eq.1) write(16,*) 'visz=',rnu,vis1,
c     $         UUZ(I+1,J,K),uu1
         END IF
         END IF
C
  100 CONTINUE
C
      RETURN
      END
