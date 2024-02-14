      SUBROUTINE BCTIDE(PP,UU,VV,WW,RHOW,FF,HH,PATM,ZC,KF,KP,KG,
     $                  INDU,INDV,INDW)
C======================================================================
C     開境界におけるPP,HHの境界条件を設定する
C
C     PP: 圧力(Pa)、このうち圧力境界セルの圧力値を更新する
C     KP: 圧力境界条件が適用されはじめるセルのz方向セルインデックス
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'TABLEI.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CP_NESTBC.h'
C
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),FF(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::ZC(8,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ)
C
      REAL(8)::DHH,DPP,HH0,HH1
      INTEGER::I,IDIR,IE,IH1,IH2,IS,J,JE,JS,K,KE,KFIJ,KS,L,M,N
      INTEGER::I1,J1
C
C----------------------------------------------------------------------
C     (1) 開境界の潮位を設定する
C----------------------------------------------------------------------
C
      DO 100 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
         IH1 = IOUTLT(3,N)
         IF(IH1.EQ.0) GO TO 100
C
         IF(IH1.GT.0) THEN
           HH1 = TABLE(IH1)
         ELSE
           IH2 = -IH1
           HH1 = HTIDE(1,IH2)
           DO 105 L=1,NTIDE
             HH1 = HH1+RTIDE(1,L,IH2)*COS(ROMEG(L)*TIME-RTIDE(2,L,IH2))
  105      CONTINUE
           HTIDE(2,IH2) = HH1
         END IF
CXXX 2005.03.16
         HH0 = HH1
CXXX END
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            IF(KF(IS,JS).EQ.MZ) I=IS+1
            DO 110 J=JS,JE
C XXXXX 2005.03.28
              HH1 = HH0+PATM(I,J)/(RHOW(I,J,KF(I,J))*GRAV)
C XXXXX END
              DHH = HH1-HH(I,J)
              HH(I,J) = HH1
              DPP = - DHH*RHOW(I,J,KF(I,J))*GRAV
C@@@@@ 東京湾で失敗？ @@@@@@@@@@@@@@@@
              DPP = 0.0D0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
              KFIJ = KF(I,J)
C
              IF(HH1.GT.ZC(1,KFIJ)) THEN
                FF(I,J,KFIJ) = 1.0D0
                KF(I,J) = KF(I,J)+1
                K = KF(I,J)
                FF(I,J,K) = (HH(I,J)-ZC(1,K-1))*ZC(6,K)
                PP(I,J,K) = PATM(I,J)
     1                    - (HH(I,J)-ZC(1,K-1))*RHOW(I,J,K-1)*GRAV
                DO 120 K=KG(I,J),KF(I,J)-1
                  PP(I,J,K) = PP(I,J,K)+DPP
 120            CONTINUE
C
              ELSE IF(HH1.GT.ZC(1,KFIJ-1)) THEN
                FF(I,J,KFIJ) = (HH(I,J)-ZC(1,KFIJ-1))*ZC(6,KFIJ)
                DO 130 K=KG(I,J),KF(I,J)
                  PP(I,J,K) = PP(I,J,K)+DPP
  130           CONTINUE
C
              ELSE
                FF(I,J,KFIJ) = 0.0D0
                KF(I,J) = KF(I,J)-1
                KP(I,J) = KF(I,J)
                KFIJ = KF(I,J)
                FF(I,J,KFIJ) = (HH(I,J)-ZC(1,KFIJ-1))*ZC(6,KFIJ)
                DO 140 K=KG(I,J),KF(I,J)
                  PP(I,J,K) = PP(I,J,K)+DPP
  140           CONTINUE
              END IF
C
  110       CONTINUE
C
            IF( I.EQ.IS ) THEN
               I1 = I-1
            ELSE
               I1 = I+1
            ENDIF
            DO 111 J=JS-1,JE
            DO 111 K=KG(I,J),KF(I,J)
               IF( INDV(I,J,K).GT.0 ) VV(I,J,K) = VV(I1,J,K)
  111       CONTINUE
C
            DO 112 J=JS,JE
            DO 112 K=KG(I,J),KF(I,J)
               IF( INDW(I,J,K).GT.0 ) WW(I,J,K) = WW(I1,J,K)
  112       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            IF(KF(IS,JS).EQ.MZ) J=JS+1
            DO 150 I=IS,IE
C XXXXX 2005.03.16
              HH1 = HH0+PATM(I,J)/(RHOW(I,J,KF(I,J))*GRAV)
C XXXXX END
              DHH = HH1-HH(I,J)
              HH(I,J) = HH1
              DPP = - DHH*RHOW(I,J,KF(I,J))*GRAV
C@@@@@ 東京湾で失敗？ @@@@@@@@@@@@@@@@
              DPP = 0.0D0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
              KFIJ = KF(I,J)
C
              IF(HH1.GT.ZC(1,KFIJ)) THEN
                FF(I,J,KFIJ) = 1.0D0
                KF(I,J) = KF(I,J)+1
                K = KF(I,J)
                FF(I,J,K) = (HH(I,J)-ZC(1,K-1))*ZC(6,K)
                PP(I,J,K) = PATM(I,J)
     1                    - (HH(I,J)-ZC(1,K-1))*RHOW(I,J,K-1)*GRAV
                DO 160 K=KG(I,J),KF(I,J)-1
                  PP(I,J,K) = PP(I,J,K)+DPP
  160           CONTINUE
C
              ELSE IF(HH1.GT.ZC(1,KFIJ-1)) THEN
                FF(I,J,KFIJ) = (HH(I,J)-ZC(1,KFIJ-1))*ZC(6,KFIJ)
                DO 170 K=KG(I,J),KF(I,J)
                  PP(I,J,K) = PP(I,J,K)+DPP
  170           CONTINUE
C
              ELSE
                FF(I,J,KFIJ) = 0.0D0
                KF(I,J) = KF(I,J)-1
                KP(I,J) = KF(I,J)
                KFIJ = KF(I,J)
                FF(I,J,KFIJ) = (HH(I,J)-ZC(1,KFIJ-1))*ZC(6,KFIJ)
                DO 180 K=KG(I,J),KF(I,J)
                  PP(I,J,K) = PP(I,J,K)+DPP
  180           CONTINUE
              END IF
C
  150       CONTINUE
C
            IF( J.EQ.JS ) THEN
               J1 = J-1
            ELSE
               J1 = J+1
            ENDIF
            DO 151 I=IS-1,IE
            DO 151 K=KG(I,J),KF(I,J)
               IF( INDU(I,J,K).GT.0 ) UU(I,J,K) = UU(I,J1,K)
  151       CONTINUE
C
            DO 152 I=IS,IE
            DO 152 K=KG(I,J),KF(I,J)
               IF( INDW(I,J,K).GT.0 ) WW(I,J,K) = WW(I,J1,K)
  152       CONTINUE
C
         END IF
C
  100 CONTINUE
C
      RETURN
      END
