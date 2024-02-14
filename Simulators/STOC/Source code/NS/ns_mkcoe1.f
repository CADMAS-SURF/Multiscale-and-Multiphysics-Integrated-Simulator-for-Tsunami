      SUBROUTINE MKCOE1(AD0,AL0,XC,YC,ZC,XCP,GX,GY,GZ,GV,GV0,GVD,CMD,
     $                  INDP,INDU,INDV,INDW)
C======================================================================
C     圧力補正式の係数行列を作成する(水面を考慮しない場合)
C
C     係数は以下の表式に基づく
C         ∂           ∂ δp                   ρ  ∂γ_j u~_j
C     - ─── ( γ_j ──── ) ΔxΔyΔz = - ── ────── ΔxΔyΔz
C       ∂x_j           ∂x_j                  Δt     ∂x_j
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::AD0(MX,MY,MZ),AL0(3,MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GV0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GVD(MX,MY,MZ),CMD(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
C
      REAL(8)::XFLG,YFLG,ZFLG,GVX1,GVY1,GVZ1
      REAL(8)::GVCM0,GVCMX,GVCMY,GVCMZ
      INTEGER::I,IDIR,IE,IS,J,JE,JS,K,KE,KS,M,N
C
C
C ... 初期化
CCC      CALL ZERCLR(AD0,MXYZ,1.0D0)
      CALL ZERCLR(AD0,MXYZ,0.0D0)
      CALL ZERCLR(AL0,3*MXYZ,0.0D0)
C
C
C ... 非対角項の係数を設定
      DO 100 K=2,MZM
      DO 100 J=2,MY
      DO 100 I=2,MX
         IF( INDP(I,J,K).GT.0 ) THEN
            XFLG = 1.0D0
            YFLG = 1.0D0
            ZFLG = 1.0D0
C ......... 流速の非計算点の場合、係数を0にする
            IF( INDU(I-1,J,K).LE.0 ) XFLG = 0.0D0
            IF( INDV(I,J-1,K).LE.0 ) YFLG = 0.0D0
            IF( INDW(I,J,K-1).LE.0 ) ZFLG = 0.0D0
C
            GVCM0=GV0(I,J,K)*(GVD(I,J,K)+(1.0D0-GVD(I,J,K))*CMD(I,J,K))
            GVCMX=GV0(I-1,J,K)*(GVD(I-1,J,K)
     $    +(1.0D0-GVD(I-1,J,K))*CMD(I-1,J,K))
            GVCMY=GV0(I,J-1,K)*(GVD(I,J-1,K)
     $    +(1.0D0-GVD(I,J-1,K))*CMD(I,J-1,K))
            GVCMZ=GV0(I,J,K-1)*(GVD(I,J,K-1)
     $    +(1.0D0-GVD(I,J,K-1))*CMD(I,J,K-1))
            GVX1= ( GV(I-1,J,K)*XC(7,I-1,J) + GV(I,J,K)*XC(8,I-1,J))
     $          / ( GVCMX*XC(7,I-1,J) + GVCM0*XC(8,I-1,J) )
            GVY1= ( GV(I,J-1,K)*YC(7,J-1) + GV(I,J,K)*YC(8,J-1))
     $          / ( GVCMY*YC(7,J-1) + GVCM0*YC(8,J-1) )
            GVZ1= ( GV(I,J,K-1)*ZC(7,K-1) + GV(I,J,K)*ZC(8,K-1))
     $          / ( GVCMZ*ZC(7,K-1) + GVCM0*ZC(8,K-1) )
C
            AL0(1,I,J,K) = - GX(I-1,J,K)*XC(5,I-1,J)*YC(4,J)*ZC(4,K)
     $                   *XFLG*GVX1
            AL0(2,I,J,K) = - GY(I,J-1,K)*YC(5,J-1)*XCP(4,I,J-1)*ZC(4,K)
     $                   *YFLG*GVY1
            AL0(3,I,J,K) = - GZ(I,J,K-1)*ZC(5,K-1)*XC(4,I,J)*YC(4,J)
     $                   *ZFLG*GVZ1
         END IF
  100 CONTINUE
C
C
C ... 対角項の係数を設定
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            AD0(I,J,K) = - ( AL0(1,I,J,K) + AL0(1,I+1,J,K)
     $                 +     AL0(2,I,J,K) + AL0(2,I,J+1,K)
     $                 +     AL0(3,I,J,K) + AL0(3,I,J,K+1) )
         END IF
  200 CONTINUE
C
C
C ... 自由流入出境界の補正
      DO 300 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 310 K=KS,KE
            DO 310 J=JS,JE
               IF( INDP(I,J,K).GT.0 ) THEN
C ............ +X方向が境界
                  AD0(I,J,K)   = AD0(I,J,K)
C     $                      + 2.0D0*GX(I,J,K)*XC(5,I,J)*YC(4,J)*ZC(4,K)
     $                      + 2.0D0*GX(I,J,K)*XC(6,I,J)*YC(4,J)*ZC(4,K)
               ELSE
C ............ -X方向が境界
                  AD0(I+1,J,K) = AD0(I+1,J,K)
C     $                      + 2.0D0*GX(I,J,K)*XC(5,I,J)*YC(4,J)*ZC(4,K)
     $                      + 2.0D0*GX(I,J,K)*XC(6,I,J)*YC(4,J)*ZC(4,K)
               END IF
  310       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 320 K=KS,KE
            DO 320 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
C ............ +Y方向が境界
                  AD0(I,J,K)   = AD0(I,J,K)
C     $                      + 2.0D0*GY(I,J,K)*YC(5,J)*XCP(4,I,J)*ZC(4,K)
     $                      + 2.0D0*GY(I,J,K)*YC(6,J)*XCP(4,I,J)*ZC(4,K)
               ELSE
C ............ -Y方向が境界
                  AD0(I,J+1,K) = AD0(I,J+1,K)
C     $                      + 2.0D0*GY(I,J,K)*YC(5,J)*XCP(4,I,J)*ZC(4,K)
     $                      + 2.0D0*GY(I,J,K)*YC(6,J)*XCP(4,I,J)*ZC(4,K)
               END IF
  320       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE
            K = KS
            DO 330 J=JS,JE
            DO 330 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
C ............ +Z方向が境界
                  AD0(I,J,K)   = AD0(I,J,K)
C     $                      + 2.0D0*GZ(I,J,K)*ZC(5,K)*XC(4,I,J)*YC(4,J)
     $                      + 2.0D0*GZ(I,J,K)*ZC(6,K)*XC(4,I,J)*YC(4,J)
               ELSE
C ............ -Z方向が境界
                  AD0(I,J,K+1) = AD0(I,J,K+1)
C     $                      + 2.0D0*GZ(I,J,K)*ZC(5,K)*XC(4,I,J)*YC(4,J)
     $                      + 2.0D0*GZ(I,J,K)*ZC(6,K)*XC(4,I,J)*YC(4,J)
               END IF
  330       CONTINUE
         END IF
  300 CONTINUE
C
      RETURN
      END
C
C     メモ
C
C     自由流入出境界の場合の処理は、圧力を境界面上で固定して係数を設定
C     実際の計算を行うときは、上三角側の係数も設定し、水面近傍セルの
C     係数を修正する必要がある
