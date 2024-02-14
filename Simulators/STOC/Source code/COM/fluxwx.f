      SUBROUTINE FLUXWX(FU,UU,WW,HU,WWX,FF,TMU,XC,ZC,GV,GX,GV0,GX0,
     $                  INDU,INDW,KF)
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
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),WW(MX,MY,MZ),HU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WWX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDW(MX,MY,MZ),KF(MX,MY)
C
      REAL(8)::XC1,FF1,FF2,HH1,HH2,HH0
      REAL(8)::TMU1,TMU2,TMU0,WW1,WW2,HU1
      REAL(8)::VIS1,VIS1M,VIS1P,ADV1,ADV1M,ADV1P
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,KF1,INDU1,INDU2,INB1
C
C
      CALL ZERCLR(FU,MXYZ,0.0D0)
C
      DO 100 K=2,MZM-1
      DO 100 J=2,MYM
      DO 100 I=1,MXM
C
         KF1 = MAX(KF(I,J),KF(I+1,J))-1
         IF( K.LE.KF1 ) THEN
C
C ...... ±Xのどちらかの側が流速のZ方向成分計算点である
         IF( INDW(I,J,K).GT.0 .OR. INDW(I+1,J,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDW(I,J,K).GT.0 ) THEN
               INB1 = I
               XC1  = 2.0D0*XC(6,I,J)
            ELSE
               INB1 = I+1
               XC1  = -2.0D0*XC(6,I+1,J)
            END IF
C
            INDU1 = INDU(I,J,K  )
            INDU2 = INDU(I,J,K+1)
C
C ......... 各メッシュの層厚比を計算しておく
C ......... 壁面でのFFは勾配0条件と仮定
            IF( INDU1.EQ.-2 ) THEN
               FF1 = FF(INB1,J,K)
               HH1 = MAX(FF1-1.0D0+GV0(INB1,J,K),0.0D0)
     $            *GV(INB1,J,K)/GV0(INB1,J,K)
            ELSE
               FF1 = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
               HH1 = MAX(FF1-1.0D0+GX0(I,J,K),0.0D0)
     $            *GX(I,J,K)/GX0(I,J,K)
            END IF
C
            IF( INDU2.EQ.-2 ) THEN
               FF2 = FF(INB1,J,K+1)
               HH2 = MAX(FF2-1.0D0+GV0(INB1,J,K+1),0.0D0)
     $            *GV(INB1,J,K+1)/GV0(INB1,J,K+1)
            ELSE
               FF2 = FF(I,J,K+1)*XC(7,I,J)+FF(I+1,J,K+1)*XC(8,I,J)
               HH2 = MAX(FF2-1.0D0+GX0(I,J,K+1),0.0D0)
     $            *GX(I,J,K+1)/GX0(I,J,K+1)
            END IF
C
            HH0 = HH1*ZC(8,K)+HH2*ZC(7,K)
C
C ......... (1) 境界条件なし
            IF( INDU1.GT.0 .AND. INDU2.GT.0 ) THEN
C
C ............ 粘性項
               TMU1 = TMU(I  ,J,K)*ZC(7,K)+TMU(I  ,J,K+1)*ZC(8,K)
               TMU2 = TMU(I+1,J,K)*ZC(7,K)+TMU(I+1,J,K+1)*ZC(8,K)
               TMU0 = TMU1*XC(7,I,J)+TMU2*XC(8,I,J)
               VIS1 = HH0*(ANUH+TMU0)*((WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
     $              +                  (UU(I,J,K+1)-UU(I,J,K))*ZC(5,K))
C
C ............ 慣性項
               WW1 = WW(I,J,K)*XC(7,I,J)+WW(I+1,J,K)*XC(8,I,J)
               HU1 = HU(I,J,K)*ZC(8,K)+HU(I,J,K+1)*ZC(7,K)
               ADV1 = PARAMV2* WW1*HU1
     $              + PARAMV *(WW(I  ,J,K)*MAX(HU1,0.0D0)
     $                        +WW(I+1,J,K)*MIN(HU1,0.0D0))
C
C ......... (2) 両方とも自由流入出境界
            ELSE IF( INDU1.EQ.0 .AND. INDU2.EQ.0 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(INB1,J,K)*ZC(7,K)+TMU(INB1,J,K+1)*ZC(8,K)
               VIS1 = HH0*(ANUH+TMU0)*(UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
C
C ............ 慣性項
               HU1 = HU(I,J,K)*ZC(8,K)+HU(I,J,K+1)*ZC(7,K)
               ADV1 = WW(INB1,J,K)*HU1
C
C ......... (3) 両方とも速度固定出境界
            ELSE IF( INDU1.EQ.-1 .AND. INDU2.EQ.-1 ) THEN
C
C ............ 粘性項
               TMU0 = TMU(INB1,J,K)*ZC(7,K)+TMU(INB1,J,K+1)*ZC(8,K)
               WW1 = WW(INB1,J,K)
               WW2 = WW(INB1,J,K)
cdist          WW1 = 0.25D0*WW(INB1,J,K-1)+0.75D0*WW(INB1,J,K  )
cdist          WW2 = 0.75D0*WW(INB1,J,K  )+0.25D0*WW(INB1,J,K+1)
C
               VIS1 = (ANUH+TMU0)
     $              * (HH0        *(UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
     $              +  HH1*ZC(8,K)*(WWX(I,J,K  )-WW1)*XC1
     $              +  HH2*ZC(7,K)*(WWX(I,J,K+1)-WW2)*XC1)
C
C ............ 慣性項
               ADV1 = WWX(I,J,K  )*HU(I,J,K  )*ZC(8,K)
     $              + WWX(I,J,K+1)*HU(I,J,K+1)*ZC(7,K)
C
C ......... (4) 壁境界または2種類の境界条件が存在
            ELSE
C              (処理方針：INB1,KセルとINB1,K+1セルでそれぞれ界面の
C               運動量流束を計算して、半分ずつ和をとる)
C
C           <<< 下側界面の処理 >>>
C ............ (4.1) 境界条件なし
               IF( INDU1.GT.0 ) THEN
                  TMU0 = TMU(I,J,K)*XC(7,I,J)+TMU(I+1,J,K)*XC(8,I,J)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
     $                  + (UU(I,J,K+1)-UU(I,J,K))*ZC(5,K))
C
                  WW1 = WW(I,J,K)*XC(7,I,J)+WW(I+1,J,K)*XC(8,I,J)
                  ADV1M = PARAMV2 *WW1*HU(I,J,K)
     $                  + PARAMV *(WW(I  ,J,K)*MAX(HU(I,J,K),0.0D0)
     $                            +WW(I+1,J,K)*MIN(HU(I,J,K),0.0D0))
C
C ............ (4.2) 自由流入出境界
               ELSE IF( INDU1.EQ.0 ) THEN
                  TMU0 = TMU(INB1,J,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *(UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
C
                  ADV1M = WW(INB1,J,K)*HU(I,J,K)
C
C ............ (4.3) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU1.EQ.-1 .OR. INDU1.EQ.-2 ) THEN
                  WW1 = WW(INB1,J,K)
cdist             WW1 = 0.25D0*WW(INB1,J,K-1)+0.75D0*WW(INB1,J,K)
                  IF( WWX(I,J,K).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 = TMU(INB1,J,K)
                  VIS1M = HH1*(ANUH+TMU0)
     $                  *((WWX(I,J,K)-WW1)*XC1
     $                  + (UU(I,J,K+1)-UU(I,J,K))*ZC(5,K))
C
                  ADV1M = WWX(I,J,K)*HU(I,J,K)
C
               ELSE
                  VIS1M = 0.0D0
                  ADV1M = 0.0D0
               END IF
C
C           <<< 上側界面の処理 >>>
C ............ (4.4) 境界条件なし
               IF( INDU2.GT.0 ) THEN
                  TMU0 = TMU(I,J,K+1)*XC(7,I,J)+TMU(I+1,J,K+1)*XC(8,I,J)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((WW(I+1,J,K)-WW(I,J,K))*XC(5,I,J)
     $                  + (UU(I,J,K+1)-UU(I,J,K))*ZC(5,K))
C
                  WW1 = WW(I,J,K)*XC(7,I,J)+WW(I+1,J,K)*XC(8,I,J)
                  ADV1P = PARAMV2* WW1*HU(I,J,K+1)
     $                  + PARAMV *(WW(I  ,J,K)*MAX(HU(I,J,K+1),0.0D0)
     $                            +WW(I+1,J,K)*MIN(HU(I,J,K+1),0.0D0))
C
C ............ (4.5) 自由流入出境界
               ELSE IF( INDU2.EQ.0 ) THEN
                  TMU0 = TMU(INB1,J,K+1)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *(UU(I,J,K+1)-UU(I,J,K))*ZC(5,K)
C
                  ADV1P = WW(INB1,J,K)*HU(I,J,K+1)
C
C ............ (4.6) 速度固定出境界と(板境界を除く)壁面
               ELSE IF( INDU2.EQ.-1 .OR. INDU2.EQ.-2 ) THEN
                  WW1 = WW(INB1,J,K)
cdist             WW1 = 0.75D0*WW(INB1,J,K)+0.25D0*WW(INB1,J,K+1)
                  IF( WWX(I,J,K+1).EQ.VSLIP ) WW1 = VSLIP
                  TMU0 = TMU(INB1,J,K+1)
                  VIS1P = HH2*(ANUH+TMU0)
     $                  *((WWX(I,J,K+1)-WW1)*XC1
     $                  + (UU(I,J,K+1)-UU(I,J,K))*ZC(5,K))
C
                  ADV1P = WWX(I,J,K+1)*HU(I,J,K+1)
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
            FU(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
