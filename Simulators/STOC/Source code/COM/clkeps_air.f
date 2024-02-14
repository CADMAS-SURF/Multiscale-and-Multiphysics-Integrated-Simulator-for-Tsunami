      SUBROUTINE CLKEPS_AIR(AKA,EPA,AKNA,EPNA,UUA,VVA,WWA,HUA,HVA,HWA,
     $                      FFA,TMUA,AKBCAIR,EPBCAIR,GVA,GXA,GYA,GZA,
     $                      XC,YC,ZCA,YCOS,
     $                      INDPA,INDUA,INDVA,INDWA,
     $                      GS,FU,FV,FW,UT,VT,WT)
C======================================================================
C     高Re型k-ε乱流モデルを解く
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       TT: 新しい時刻の温度(°C),TN:古い時刻の温度(°C)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'TURBR.h'
C
      REAL(8),INTENT(OUT)::AKA(MX,MY,MZA),EPA(MX,MY,MZA)
      REAL(8),INTENT(IN)::AKNA(MX,MY,MZA),EPNA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::AKBCAIR(NXY,MZA,4),EPBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::YCOS(MY)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      REAL(8)::GS(MX,MY,MZA)
      REAL(8)::FU(MX,MY,MZA),FV(MX,MY,MZA),FW(MX,MY,MZA)
      REAL(8)::UT(MX,MY,MZA),VT(MX,MY,MZA),WT(MX,MY,MZA)
C
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     生成項を計算する
C----------------------------------------------------------------------
      CALL CLKEGN_AIR(GS,UUA,VVA,WWA,TMUA,XC,YC,ZCA,
     $                INDUA,INDVA,INDWA,INDPA,UT,VT,WT)
C
C----------------------------------------------------------------------
C     kの方程式を計算する
C----------------------------------------------------------------------
      CALL CLKEQ_AIR(AKA,AKNA,EPNA,HUA,HVA,HWA,FFA,TMUA,AKBCAIR,
     $               GVA,GXA,GYA,GZA,XC,YC,ZCA,YCOS,
     $               INDPA,INDUA,INDVA,INDWA,GS,FU,FV,FW)
C
C----------------------------------------------------------------------
C     εの方程式を計算する
C----------------------------------------------------------------------
      CALL CLEEQ_AIR(EPA,AKNA,EPNA,HUA,HVA,HWA,FFA,TMUA,EPBCAIR,
     $               GVA,GXA,GYA,GZA,XC,YC,ZCA,YCOS,
     $               INDPA,INDUA,INDVA,INDWA,GS,FU,FV,FW)
C
C----------------------------------------------------------------------
C     壁境界条件を計算する
C----------------------------------------------------------------------
      CALL WALBND_AIR(AKA,EPA,UUA,VVA,WWA,XC,YC,ZCA,
     $               INDPA,INDUA,INDVA,INDWA,UT,VT,WT)
C
C----------------------------------------------------------------------
C     計算値をチェックする
C----------------------------------------------------------------------
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF(INDPA(I,J,K).GT.0) THEN
            IF(EPA(I,J,K).LT.EPMIN) THEN
               EPA(I,J,K) = EPMIN
               AKA(I,J,K) = AKMIN
            ELSE IF(AKA(I,J,K).LT.AKMIN) THEN
               AKA(I,J,K) = AKMIN
            END IF
         ELSE
            AKA(I,J,K) = 0.0D0
            EPA(I,J,K) = 0.0D0
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
