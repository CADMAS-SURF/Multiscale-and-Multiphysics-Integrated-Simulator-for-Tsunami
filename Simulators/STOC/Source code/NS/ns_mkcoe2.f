      SUBROUTINE MKCOE2(AD,AL,AU,BB,AD0,AL0,UU,VV,WW,PP,RHOW,PATM,DPS,
     $                 XC,YC,ZC,XCP,GX,GY,GZ,HH,INDU,INDV,INDP,KF,KG,KP)
C======================================================================
C     圧力補正式の係数行列と右辺を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::AD(MX,MY,MZ),AL(3,MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AU(3,MX,MY,MZ),BB(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::AD0(MX,MY,MZ),AL0(3,MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::DPS(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY)
C
      REAL(8)::BBEPS,BBSUM,VVSUM,ZCD
      INTEGER::I,ISUM,J,K,NPFX
C
C
      CALL ZERCLR(AU,3*MXYZ,0.0D0)
      CALL ZERCLR(BB,MXYZ,0.0D0)
C
C
C ... (1) 設定済みの値をコピー
      DO 100 K=1,MZ
      DO 100 J=1,MY
      DO 100 I=1,MX
         AD(I,J,K)   = AD0(I,J,K)
         AL(1,I,J,K) = AL0(1,I,J,K)
         AL(2,I,J,K) = AL0(2,I,J,K)
         AL(3,I,J,K) = AL0(3,I,J,K)
         IF( I.GT.1 .AND. J.GT.1 .AND. K.GT.1 ) THEN
            AU(1,I-1,J,K) = AL0(1,I,J,K)
            AU(2,I,J-1,K) = AL0(2,I,J,K)
            AU(3,I,J,K-1) = AL0(3,I,J,K)
         END IF
  100 CONTINUE
C
C
      IF( LSURF.EQ.1 ) THEN
C
C ... (2) 上部気体セルを計算対象から外す
      DO 200 J=2,MYM
      DO 200 I=2,MXM
C ...... 水面のある領域のみ計算
         IF( KF(I,J).LT.MZ ) THEN
            DO 210 K=KF(I,J)+1,MZM
               AD(I,J,K)   = 1.0D0
               AL(1,I,J,K) = 0.0D0
               AL(2,I,J,K) = 0.0D0
               AL(3,I,J,K) = 0.0D0
               AU(1,I,J,K) = 0.0D0
               AU(2,I,J,K) = 0.0D0
               AU(3,I,J,K) = 0.0D0
  210       CONTINUE
         END IF
  200 CONTINUE
C
C
C ... (3) 圧力境界セルの係数を設定する
      DO 300 J=2,MYM
      DO 300 I=2,MXM
      DO 310 K=KF(I,J),KP(I,J),-1
        IF( K.EQ.KF(I,J) ) THEN
          IF(K.GT.KG(I,J) ) THEN
            ZCD = ZC(3,K-1)/(HH(I,J)-ZC(2,K-1))
            AD(I,J,K)   = 1.0D0
            AL(1,I,J,K) = 0.0D0
            AL(2,I,J,K) = 0.0D0
            AL(3,I,J,K) = ZCD-1.0D0
            AU(1,I,J,K) = 0.0D0
            AU(2,I,J,K) = 0.0D0
            AU(3,I,J,K) = 0.0D0
            BB(I,J,K) = PP(I,J,K-1)*(1.0D0-ZCD)-PP(I,J,K)+ZCD*PATM(I,J)
          ELSE
            AD(I,J,K)   = 1.0D0
            AL(1,I,J,K) = 0.0D0
            AL(2,I,J,K) = 0.0D0
            AL(3,I,J,K) = 0.0D0
            AU(1,I,J,K) = 0.0D0
            AU(2,I,J,K) = 0.0D0
            AU(3,I,J,K) = 0.0D0
          END IF
        ELSE
          IF(INDP(I,J,K).GT.0) THEN
            IF(K.GT.KF(I+1,J).AND.INDU(I,J,K).GT.0) THEN
              AU(1,I,J,K) = 0.0D0
            END IF
            IF(K.GT.KF(I-1,J).AND.INDU(I-1,J,K).GT.0) THEN
              AL(1,I,J,K) = 0.0D0
            END IF
            IF(K.GT.KF(I,J+1).AND.INDV(I,J,K).GT.0) THEN
              AU(2,I,J,K) = 0.0D0
            END IF
            IF(K.GT.KF(I,J-1).AND.INDV(I,J-1,K).GT.0) THEN
              AL(2,I,J,K) = 0.0D0
            END IF
C
            BB(I,J,K) = - RHO / DTV
     $            * ( ( GX(I  ,J,K)*UU(I  ,J,K)
     $            -     GX(I-1,J,K)*UU(I-1,J,K) ) *YC(4,J)*ZC(4,K)
     $            +   ( GY(I,J  ,K)*VV(I,J  ,K)*XCP(4,I,J  )
     $            -     GY(I,J-1,K)*VV(I,J-1,K)*XCP(4,I,J-1) ) *ZC(4,K)
     $            +   ( GZ(I,J,K  )*WW(I,J,K  )
     $            -     GZ(I,J,K-1)*WW(I,J,K-1) ) *XC(4,I,J)*YC(4,J) )
          END IF
        END IF
  310 CONTINUE
  300 CONTINUE
C
      END IF
C     ...... 自由表面処理終了
C
C ... (4) 右辺を設定する
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C
         DO 410 K=KG(I,J),KP(I,J)-1
            IF( INDP(I,J,K).GT.0 ) THEN
               BB(I,J,K) = - RHO / DTV
     $            * ( ( GX(I  ,J,K)*UU(I  ,J,K)
     $            -     GX(I-1,J,K)*UU(I-1,J,K) ) *YC(4,J)*ZC(4,K)
     $            +   ( GY(I,J  ,K)*VV(I,J  ,K)*XCP(4,I,J  )
     $            -     GY(I,J-1,K)*VV(I,J-1,K)*XCP(4,I,J-1) ) *ZC(4,K)
     $            +   ( GZ(I,J,K  )*WW(I,J,K  )
     $            -     GZ(I,J,K-1)*WW(I,J,K-1) ) *XC(4,I,J)*YC(4,J) )
            END IF
  410    CONTINUE
  400 CONTINUE
C
C
C ... (5) 右辺を修正する
      IF(LSURF.EQ.0.AND.NOUTLT.EQ.0) THEN
C
        isum=1
        if(isum.ne.0) then
        BBSUM = 0.0D0
        VVSUM = 0.0D0
        DO 500 J=2,MYM
        DO 500 I=2,MXM
          DO 510 K=KG(I,J),KP(I,J)-1
            IF( INDP(I,J,K).GT.0 ) THEN
              BBSUM = BBSUM+BB(I,J,K)
              VVSUM = VVSUM+XC(4,I,J)*YC(4,J)*ZC(4,K)
            END IF
  510     CONTINUE
  500   CONTINUE
C
        BBEPS = BBSUM/VVSUM
        NPFX = 0
        DO 520 J=2,MYM
        DO 520 I=2,MXM
          DO 530 K=KG(I,J),KP(I,J)-1
            IF( INDP(I,J,K).GT.0 ) THEN
              BB(I,J,K) = BB(I,J,K)-BBEPS*XC(4,I,J)*YC(4,J)*ZC(4,K)
              BBSUM = BBSUM+BB(I,J,K)
              IF(NPFX.EQ.0) THEN
                AD(I,J,K) = 1.0D50
                BB(I,J,K) = 0.0D0
                NPFX = 1
              END IF
            END IF
  530     CONTINUE
  520   CONTINUE
        end if
C
      END IF
C
      RETURN
      END
