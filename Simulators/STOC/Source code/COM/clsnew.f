      SUBROUTINE CLSNEW(CC,SN,FU,FV,FW,SRCA,SRCB,TMUZ,HH,
     $                  XC,YC,ZC,XCP,YCOS,GV,GZ,
     $                  INDP,INDU,INDV,KG,KP,KF,ANUD,DTMU,ICOUPL)
C======================================================================
C     スカラーの輸送方程式を解き、スカラー変数を更新する
C     SN: セル内のスカラー変数の保存量({温度,塩分濃度,物質量等}*流体体積)
C     FU: X方向格子点、Y方向セル中心点、Z方向セル中心点で定義
C     FV: X方向セル中心点、Y方向格子点、Z方向セル中心点で定義
C     FW: X方向セル中心点、Y方向セル中心点、Z方向格子点で定義
C
CCCC     SVAL     表面境界値
CCCC     ISFLG    表面境界条件(=0:勾配0, =1:固定)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(INOUT)::CC(MX,MY,MZ),SN(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),YCOS(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GZ(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KG(MX,MY),KP(MX,MY),KF(MX,MY)
      INTEGER,INTENT(IN)::ICOUPL
      REAL(8),INTENT(IN)::ANUD,DTMU
C
      INTEGER::IDB=0
      INTEGER::I,J,K,KF1
      REAL(8)::VOL,DEP,SSS,DH,H1,DTS
C
C
CCC     CALL ZERCLR(CC,MXYZ,0.0D0)
C
C ... 水面なし
      IF(LSURF.EQ.0) THEN  
        DO 100 K=2,MZM
        DO 100 J=2,MYM
        DO 100 I=2,MXM
          IF( INDP(I,J,K).GT.0 ) THEN
             DTS = DT/(1.0D0+DT*SRCA(I,J,K))
             CC(I,J,K) = SN(I,J,K)/(XC(4,I,J)*YC(4,J)*ZC(4,K)*GV(I,J,K))
     1                 + DTS*((FU(I,J,K)-FU(I-1,J,K))*XC(6,I,J)
     2                       +(FV(I,J,K)-FV(I,J-1,K))*YC(6,J)/YCOS(J)
     3                       +(FW(I,J,K)-FW(I,J,K-1))*ZC(6,K))/GV(I,J,K)
     4                 + DTS*SRCB(I,J,K)
          END IF
  100   CONTINUE
C
C ... 水面あり
      ELSE     
        DO 200 K=2,MZM
        DO 200 J=2,MYM
        DO 200 I=2,MXM
C
          IF(KF(I,J).LT.MZ) THEN
C
C ......... 底層から表面セルの2つ下までのスカラー量を計算する
            IF( INDP(I,J,K).GT.0.AND.K.LT.KF(I,J)-1 ) THEN
              DTS = DT/(1.0D0+DT*SRCA(I,J,K))
              CC(I,J,K) =SN(I,J,K)/(XC(4,I,J)*YC(4,J)*ZC(4,K)*GV(I,J,K))
     1                 + DTS*((FU(I,J,K)-FU(I-1,J,K))*XC(6,I,J)
     2                       +(FV(I,J,K)-FV(I,J-1,K))*YC(6,J)/YCOS(J)
     3                       +(FW(I,J,K)-FW(I,J,K-1))*ZC(6,K))/GV(I,J,K)
     4                 + DTS*SRCB(I,J,K)
            END IF
          END IF
  200   CONTINUE
C
C ..... 表面セルと一つ下のセル、上空セルのスカラー量を計算する
C       特殊なケースとして、(a) 表面セルが地面に接している場合(KF=KG)。このときは一つ下のセルがない
C                           (b) 表面セルの層厚がほぼ0の場合
C
        DO 210 J=2,MYM
        DO 210 I=2,MXM         
          IF(KF(I,J).LT.MZ) THEN
C
C ......... (1) 時間積分処理(鉛直方向ループ)
            KF1 = KF(I,J)
            SSS = 0.0D0
C
            DO 220 K=KF1-1,MZM
              IF(INDP(I,J,K).GT.0) THEN
                DH = ZC(4,K)*GV(I,J,K)
                IF(K.EQ.KF1) THEN
                  IF(K.EQ.KG(I,J)) THEN
                    DEP = ZC(1,K)-GV(I,J,K)*ZC(4,K)
                    DH = HH(I,J)-DEP
                  ELSE
c                    DH = (HH(I,J)-ZC(1,K-1))*GV(I,J,K)
                    DH = HH(I,J)-ZC(1,K-1)
                  END IF
                ELSE IF(K.GT.KF1) THEN
CC h^n w^n+1/2でフラックスを計算するために、水面より上でもFWが有意な値をもつことがありうるため。コメントアウト
CC これと関連して、bctsrf.fとbccsrf.fで計算する表面フラックスは上方向にコピーするように修正
                    DH = 0.0D0
                END IF
C
                DTS = DT/(1.0D0+DT*SRCA(I,J,K))
                SSS = SSS+SN(I,J,K)
     1              + DTS*((FU(I,J,K)-FU(I-1,J,K))*YC(4,J)*ZC(4,K)
     2                    +(FV(I,J,K)*XCP(4,I,J)
     $                     -FV(I,J-1,K)*XCP(4,I,J-1))*ZC(4,K)
     $                    +(FW(I,J,K)-FW(I,J,K-1))*XC(4,I,J)*YC(4,J))
     $              + DTS*SRCB(I,J,K)*XC(4,I,J)*YC(4,J)*DH
              END IF
  220       CONTINUE
C
c move from (120319) 開境界の処理
C
C ......... (2) 表層2層の均質化
            IF(KF1.EQ.KG(I,J)) THEN
              DEP = ZC(1,KF1)-GV(I,J,KF1)*ZC(4,KF1)
              VOL = XC(4,I,J)*YC(4,J)*(HH(I,J)-DEP)
              IF(VOL.NE.0.0D0) THEN
                CC(I,J,KF1) = SSS/VOL
              ELSE
                CC(I,J,KF1) = 0.0D0
              END IF
            ELSE
c move to (120319)
C ......... 開境界の処理
              IF(INDU(I-1,J,KF1).EQ.0.OR.INDU(I,J,KF1).EQ.0.OR.
     $           INDV(I,J-1,KF1).EQ.0.OR.INDV(I,J,KF1).EQ.0) THEN
                CC(I,J,KF1) = CC(I,J,KF1-1)
                IF(KF1+1.LE.MZM) CC(I,J,KF1+1) = CC(I,J,KF1)
                GO TO 210
              END IF
C
              VOL = XC(4,I,J)*YC(4,J)*ZC(4,KF1-1)*GV(I,J,KF1-1)
     $            + XC(4,I,J)*YC(4,J)*(HH(I,J)-ZC(1,KF1-1))
              CC(I,J,KF1-1) = SSS/VOL
              CC(I,J,KF1  ) = SSS/VOL
            END IF
C
            CC(I,J,KF1+1) = CC(I,J,KF1)
          END IF
  210   CONTINUE
      END IF
C
C ... 鉛直方向陰解法の場合の処理
      IF( IMVERT.NE.0.AND.ICOUPL.EQ.0 ) THEN
         CALL CLSIMP(CC,TMUZ,HH,ZC,GV,GZ,KG,KF,ANUD,DTMU)
      ENDIF
C
      RETURN
      END
