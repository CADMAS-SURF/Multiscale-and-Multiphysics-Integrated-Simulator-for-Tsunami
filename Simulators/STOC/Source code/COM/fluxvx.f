      SUBROUTINE FLUXVX(FU,UU,VV,HU,VVX,FF,TMU,XC,YC,XCP,GV,GX,GV0,GX0,
     $                  INDU,INDV,KF)
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
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),HU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::VVX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),XCP(8,MX,MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ),KF(MX,MY)
C
      REAL(8)::XC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::TMU1,TMU2,TMU0,VV1,VV2,HU1
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDU1,INDU2,INB1
C
C
      CALL ZERCLR(FU,MXYZ,0.0D0)
C
      DO 100 K=2,MZM
C     DO 100 J=2,MYM-1
C                   ( for DOMAIN-DECOMP )
      DO 100 J=2,MYM
      DO 100 I=1,MXM
C
         KF1 = MAX(KF(I,J),KF(I+1,J),KF(I,J+1),KF(I+1,J+1))
         KF1 = MIN(KF1,MZM)
         IF( K.LE.KF1 ) THEN
C
C ...... ±Xのどちらかの側が流速のY方向成分計算点である
         IF( INDV(I,J,K).GT.0 .OR. INDV(I+1,J,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDV(I,J,K).GT.0 ) THEN
               INB1 = I
               XC1  = 2.0D0*XCP(6,I,J)
            ELSE
               INB1 = I+1
               XC1  = -2.0D0*XCP(6,I+1,J)
            END IF
C
            INDU1 = INDU(I,J  ,K)
            INDU2 = INDU(I,J+1,K)
C
C ......... 各メッシュの層厚比を計算しておく
cxxxxxxxxxx FF1 = FF(I,J  ,K)*XC(7,I,J  )+FF(I+1,J  ,K)*XC(8,I,J  )
cxxxxxxxxxx FF2 = FF(I,J+1,K)*XC(7,I,J+1)+FF(I+1,J+1,K)*XC(8,I,J+1)
C ......... 壁面でのFFは勾配0条件と仮定
cxxxxxxxxxx IF( INDU1.EQ.-2 ) FF1 = FF(INB1,J  ,K)
cxxxxxxxxxx IF( INDU2.EQ.-2 ) FF2 = FF(INB1,J+1,K)
            FF1 = MIN(FF(I,J  ,K),FF(I+1,J  ,K))
            FF2 = MIN(FF(I,J+1,K),FF(I+1,J+1,K))
            HH1 = MAX(FF1-1.0D0+GX0(I,J  ,K),0.0D0)
     $         *GX(I,J  ,K)/GX0(I,J  ,K)
            HH2 = MAX(FF2-1.0D0+GX0(I,J+1,K),0.0D0)
     $         *GX(I,J+1,K)/GX0(I,J+1,K)
            IF( INDU1.EQ.-2 ) HH1 = MAX(FF1-1.0D0+GV0(INB1,J  ,K),0.0D0)
     $         *GV(INB1,J  ,K)/GV0(INB1,J  ,K)
            IF( INDU2.EQ.-2 ) HH2 = MAX(FF2-1.0D0+GV0(INB1,J+1,K),0.0D0)
     $         *GV(INB1,J+1,K)/GV0(INB1,J+1,K)
            HH0 = HH1*YC(8,J)+HH2*YC(7,J)
C
C ......... (1) 境界条件なし
            IF( INDU1.GT.0 .AND. INDU2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1 = TMU(I  ,J,K)*YC(7,J)+TMU(I  ,J+1,K)*YC(8,J)
               TMU2 = TMU(I+1,J,K)*YC(7,J)+TMU(I+1,J+1,K)*YC(8,J)
               TMU0 = TMU1*XCP(7,I,J)+TMU2*XCP(8,I,J)
               VIS1 =HH0*(ANUH+TMU0)*((VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
     $              +                  (UU(I,J+1,K)-UU(I,J,K))*YC(5,J))
C
C ............ 慣性項
               VV1 = VV(I,J,K)*XCP(7,I,J)+VV(I+1,J,K)*XCP(8,I,J)
               HU1 = HU(I,J,K)*YC(8,J)+HU(I,J+1,K)*YC(7,J)
               ADV1 = PARAMV2* VV1*HU1
     $              + PARAMV *(VV(I  ,J,K)*MAX(HU1,0.0D0)
     $                        +VV(I+1,J,K)*MIN(HU1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDU1.EQ.0 .AND. INDU2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(INB1,J,K)*YC(7,J)+TMU(INB1,J+1,K)*YC(8,J)
               VIS1 = HH0*(ANUH+TMU0)*(UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
C
C ............ 慣性項
               HU1 = HU(I,J,K)*YC(8,J)+HU(I,J+1,K)*YC(7,J)
               ADV1 = VV(INB1,J,K)*HU1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDU1.EQ.-1 .AND. INDU2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(INB1,J,K)*YC(7,J)+TMU(INB1,J+1,K)*YC(8,J)
               VV1 = VV(INB1,J,K)
               VV2 = VV(INB1,J,K)
cdist          VV1 = 0.25D0*VV(INB1,J-1,K)+0.75D0*VV(INB1,J  ,K)
cdist          VV2 = 0.75D0*VV(INB1,J  ,K)+0.25D0*VV(INB1,J+1,K)
C
               VIS1 = (ANUH+TMU0)
     $              * (HH0        *(UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
     $              +  HH1*YC(8,J)*(VVX(I,J  ,K)-VV1)*XC1
     $              +  HH2*YC(7,J)*(VVX(I,J+1,K)-VV2)*XC1)
C
C ............ 慣性項
               ADV1 = VVX(I,J  ,K)*HU(I,J  ,K)*YC(8,J)
     $              + VVX(I,J+1,K)*HU(I,J+1,K)*YC(7,J)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：INB1,JセルとINB1,J+1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 左側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDU1.GT.0 ) THEN
                  TMU0 = TMU(I,J,K)*XC(7,I,J)+TMU(I+1,J,K)*XC(8,I,J)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
     $                  + (UU(I,J+1,K)-UU(I,J,K))*YC(5,J))
C
                  VV1 = VV(I,J,K)*XCP(7,I,J)+VV(I+1,J,K)*XCP(8,I,J)
                  ADV1M = PARAMV2 *VV1*HU(I,J,K)
     $                  + PARAMV *(VV(I  ,J,K)*MAX(HU(I,J,K),0.0D0)
     $                            +VV(I+1,J,K)*MIN(HU(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDU1.EQ.0 ) THEN
                  TMU0 = TMU(INB1,J,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *(UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
C
                  ADV1M = VV(INB1,J,K)*HU(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU1.EQ.-1 .OR. INDU1.EQ.-2 ) THEN
                  VV1 = VV(INB1,J,K)
cdist             VV1 = 0.25D0*VV(INB1,J-1,K)+0.75D0*VV(INB1,J,K)
                  IF( VVX(I,J,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 = TMU(INB1,J,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((VVX(I,J,K)-VV1)*XC1
     $                  + (UU(I,J+1,K)-UU(I,J,K))*YC(5,J))
C
                  ADV1M = VVX(I,J,K)*HU(I,J,K)
C
               ELSE
                  VIS1M = 0.0D0
                  ADV1M = 0.0D0
               END IF
C
C           <<< 右側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDU2.GT.0 ) THEN
                  TMU0 = TMU(I  ,J+1,K)*XC(7,I,J+1)
     $                  +TMU(I+1,J+1,K)*XC(8,I,J+1)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
     $                  + (UU(I,J+1,K)-UU(I,J,K))*YC(5,J))
C
                  VV1 = VV(I,J,K)*XCP(7,I,J)+VV(I+1,J,K)*XCP(8,I,J)
                  ADV1P = PARAMV2* VV1*HU(I,J+1,K)
     $                  + PARAMV *(VV(I  ,J,K)*MAX(HU(I,J+1,K),0.0D0)
     $                            +VV(I+1,J,K)*MIN(HU(I,J+1,K),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDU2.EQ.0 ) THEN
                  TMU0 = TMU(INB1,J+1,K)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *(UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
C
                  ADV1P = VV(INB1,J,K)*HU(I,J+1,K)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU2.EQ.-1 .OR. INDU2.EQ.-2 ) THEN
                  VV1 = VV(INB1,J,K)
cdist             VV1 = 0.75D0*VV(INB1,J,K)+0.25D0*VV(INB1,J+1,K)
                  IF( VVX(I,J+1,K).EQ.VSLIP ) VV1 = VSLIP
                  TMU0 = TMU(INB1,J+1,K)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((VVX(I,J+1,K)-VV1)*XC1
     $                  + (UU(I,J+1,K)-UU(I,J,K))*YC(5,J))
C
                  ADV1P = VVX(I,J+1,K)*HU(I,J+1,K)
C
               ELSE
                  VIS1P = 0.0D0
                  ADV1P = 0.0D0
               END IF
C
               VIS1 = VIS1M*YC(8,J) + VIS1P*YC(7,J)
               ADV1 = ADV1M*YC(8,J) + ADV1P*YC(7,J)
            END IF
C
            IF(ISW(4).NE.0) ADV1=0.0D0
C
            FU(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
