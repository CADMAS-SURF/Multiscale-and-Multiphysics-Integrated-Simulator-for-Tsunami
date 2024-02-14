      SUBROUTINE FLUXVY(FV,VV,HV,TMU,FF,YC,GV,GY,GV0,GY0,INDV,LLWALB,KF)
C======================================================================
C     Y方向の運動量保存式のY方向界面の運動量流束を計算する
C     FV: セル中心定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::FV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::VV(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::YC(8,MY),GV(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GY0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY)
C
      REAL(8)::GY1,HH1,VIS1,VV1,HV1,ADV1,HH2
      INTEGER::I,J,K,N
C     越流部の粘性を2層に分けて計算するか否か(=0:しない、=1:する)
      INTEGER,PARAMETER:: IVIS2=0
C
C
      CALL ZERCLR(FV,MXYZ,0.0D0)
C
      IF( IVIS2.EQ.1 ) THEN
C ...... 防潮堤用の処理(Y方向)
!CDIR NODEP
         DO N=MLWALBX+1,MLWALB
            I = LLWALB(1,N)
            J = LLWALB(2,N)
            K = LLWALB(3,N)
C
C ......... LEFT SIDE
            IF( K.LE.KF(I,J) ) THEN
C ............ 粘性項
               IF( GY0(I,J,K).LT.GV0(I,J,K) ) THEN
                  HH1  = MAX(FF(I,J,K)-1.0D0+GV0(I,J,K),0.0D0)
                  HH2  = MAX(FF(I,J,K)-1.0D0+GY0(I,J,K),0.0D0)
                  VIS1 = (ANUH+TMU(I,J,K))*2.0D0*YC(6,J)
     $                 *( HH2     *(VV(I,J,K)-VV(I,J-1,K))
     $                 + (HH1-HH2)*(         -VV(I,J-1,K)))
                  FV(I,J,K) = VIS1 *GV(I,J,K)/GV0(I,J,K)
               ENDIF
            ENDIF
C
            J=J+1
C ......... RIGHT SIDE
            IF( K.LE.KF(I,J) ) THEN
C ............ 粘性項
               IF( GY0(I,J-1,K).LT.GV0(I,J,K) ) THEN
                  HH1  = MAX(FF(I,J,K)-1.0D0+GV0(I,J,K),0.0D0)
                  HH2  = MAX(FF(I,J,K)-1.0D0+GY0(I,J-1,K),0.0D0)
                  VIS1 = (ANUH+TMU(I,J,K))*2.0D0*YC(6,J)
     $                 *( HH2     *(VV(I,J,K)-VV(I,J-1,K))
     $                 + (HH1-HH2)*(VV(I,J,K)            ))
                  FV(I,J,K) = VIS1 *GV(I,J,K)/GV0(I,J,K)
               ENDIF
            ENDIF
         ENDDO
      ENDIF
C
C
      DO 100 K=2,MZM
C     DO 100 J=2,MYM
C                   ( for DOMAIN-DECOMP )
      DO 100 J=2,MY
      DO 100 I=2,MXM
C
         IF( K.LE.KF(I,J) ) THEN
         IF( INDV(I,J-1,K).GT.0 .OR. INDV(I,J,K).GT.0 ) THEN
C
C ......... 粘性項
            VIS1=0.0D0
            IF( FV(I,J,K).EQ.0.0D0 ) THEN
C           GY1  = 0.5D0*(GY(I,J-1,K)+GY(I,J,K))
C           (壁面でGX=1を回避)
            GY1  = GV0(I,J,K)
            HH1  = MAX(FF(I,J,K)-1.0D0+GY1,0.0D0)
     $           * GV(I,J,K)/GV0(I,J,K)
C
            VIS1 = HH1*(ANUH+TMU(I,J,K))*2.0D0
     $           *(VV(I,J,K)-VV(I,J-1,K))*YC(6,J)
            ENDIF
C
C ......... 慣性項
            VV1  = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            HV1  = 0.5D0*(HV(I,J-1,K)+HV(I,J,K))
C
            ADV1 = PARAMV2* VV1*HV1
     $           + PARAMV *(VV(I,J-1,K)*MAX(HV1,0.0D0)
     $                     +VV(I,J  ,K)*MIN(HV1,0.0D0))
            IF(ISW(4).NE.0) ADV1=0.0D0
C
            FV(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
