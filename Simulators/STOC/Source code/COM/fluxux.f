      SUBROUTINE FLUXUX(FU,UU,HU,TMU,FF,XC,GV,GX,GV0,GX0,INDU,LLWALB,KF)
C======================================================================
C     X方向の運動量保存式のX方向界面の運動量流束を計算する
C     FU: セル中心定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),HU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY)
C
      REAL(8)::GX1,HH1,VIS1,UU1,HU1,ADV1,HH2
      INTEGER::I,J,K,N
C     越流部の粘性を2層に分けて計算するか否か(=0:しない、=1:する)
      INTEGER,PARAMETER:: IVIS2=0
C
C
      CALL ZERCLR(FU,MXYZ,0.0D0)
C
      IF( IVIS2.EQ.1 ) THEN
C ...... 防潮堤用の処理(X方向)
!CDIR NODEP
         DO N=1,MLWALBX
            I = LLWALB(1,N)
            J = LLWALB(2,N)
            K = LLWALB(3,N)
C
C ......... LEFT SIDE
            IF( K.LE.KF(I,J) ) THEN
C ............ 粘性項
               IF( GX0(I,J,K).LT.GV0(I,J,K) ) THEN
                  HH1  = MAX(FF(I,J,K)-1.0D0+GV0(I,J,K),0.0D0)
                  HH2  = MAX(FF(I,J,K)-1.0D0+GX0(I,J,K),0.0D0)
                  VIS1 = (ANUH+TMU(I,J,K))*2.0D0*XC(6,I,J)
     $                 *( HH2     *(UU(I,J,K)-UU(I-1,J,K))
     $                 + (HH1-HH2)*(         -UU(I-1,J,K)))
                  FU(I,J,K) = VIS1 *GV(I,J,K)/GV0(I,J,K)
               ENDIF
            ENDIF
C
            I=I+1
C ......... RIGHT SIDE
            IF( K.LE.KF(I,J) ) THEN
C ............ 粘性項
               IF( GX0(I-1,J,K).LT.GV0(I,J,K) ) THEN
                  HH1  = MAX(FF(I,J,K)-1.0D0+GV0(I,J,K),0.0D0)
                  HH2  = MAX(FF(I,J,K)-1.0D0+GX0(I-1,J,K),0.0D0)
                  VIS1 = (ANUH+TMU(I,J,K))*2.0D0*XC(6,I,J)
     $                 *( HH2     *(UU(I,J,K)-UU(I-1,J,K))
     $                 + (HH1-HH2)*(UU(I,J,K)            ))
                  FU(I,J,K) = VIS1 *GV(I,J,K)/GV0(I,J,K)
               ENDIF
            ENDIF
         ENDDO
      ENDIF
C
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
C     DO 100 I=2,MXM
C                   ( for DOMAIN-DECOMP )
      DO 100 I=2,MX
C
         IF( K.LE.KF(I,J) ) THEN
         IF( INDU(I-1,J,K).GT.0 .OR. INDU(I,J,K).GT.0 ) THEN
C
C ......... 粘性項
            VIS1=0.0D0
            IF( FU(I,J,K).EQ.0.0D0 ) THEN
C           GX1  = 0.5D0*(GX(I-1,J,K)+GX(I,J,K))
C           (壁面でGX=1を回避)
            GX1  = GV0(I,J,K)
            HH1  = MAX(FF(I,J,K)-1.0D0+GX1,0.0D0)
     $           * GV(I,J,K)/GV0(I,J,K)
C
            VIS1 = HH1*(ANUH+TMU(I,J,K))*2.0D0
     $           *(UU(I,J,K)-UU(I-1,J,K))*XC(6,I,J)
            ENDIF
C
C ......... 慣性項
            UU1  = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            HU1  = 0.5D0*(HU(I-1,J,K)+HU(I,J,K))
C
            ADV1 = PARAMV2* UU1*HU1
     $           + PARAMV *(UU(I-1,J,K)*MAX(HU1,0.0D0)
     $                     +UU(I  ,J,K)*MIN(HU1,0.0D0))
            IF(ISW(4).NE.0) ADV1=0.0D0
C
            FU(I,J,K) = VIS1 - ADV1
          END IF
          END IF
  100 CONTINUE
C
      RETURN
      END
