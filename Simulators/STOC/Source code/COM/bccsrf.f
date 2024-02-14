      SUBROUTINE BCCSRF(SRCA,SRCB,FW,CC,HH,HX,HDEP,QW,XC,YC,ZC,GV,
     $                  INDP,KF,KH,KG)
C======================================================================
C
C     濃度輸送式のZ方向流束に降雨蒸発条件を加える
C       FW: X方向セル中心点、Y方向セル中心点、Z方向格子点で定義
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CONSRV.h'
C
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FW(MX,MY,MZ),CC(MX,MY,MZ),GV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::QW(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KH(MX,MY),KG(MX,MY)
C
      REAL(8)::QVOL,VOL,Q1,Q2,DH
      INTEGER::I,J,K,KF1
C
C----------------------------------------------------------------------
C     SURFACE-TYPE = FUNCTIONの場合
C----------------------------------------------------------------------
      IF(ISURF(1).EQ.-1) THEN
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
      IF(KH(I,J).LT.MZ.AND.QW(I,J).NE.0.0D0) THEN
C
        KF1 = KH(I,J)
        IF(INDP(I,J,KF1-1).GT.0) THEN
C
C ....... 表層部の体積(高さ)を合計する
          VOL = 0.0D0
          DO 110 K=KF1-1,KH(I,J)
            IF(K.EQ.KH(I,J)) THEN
              VOL = VOL+GV(I,J,K)*(HX(I,J)-ZC(1,K-1))
            ELSE
              VOL = VOL+GV(I,J,K)*ZC(4,K)
            END IF
  110     CONTINUE
C
C ....... セルの消滅量として処理する
          Q1 = QW(I,J)*ZC(4,KF1-1)/VOL
          Q2 = QW(I,J)*(HX(I,J)-ZC(1,KF1-1))/VOL
          IF(VOL.GT.0.0D0) THEN
CC            FW(I,J,KF1-1) = FW(I,J,KF1-1)+Q1
CC            FW(I,J,KF1  ) = FW(I,J,KF1  )+QW(I,J)
            FW(I,J,KF1-1) = FW(I,J,KF1-1)+Q1*CC(I,J,KF1-1)
            FW(I,J,KF1  ) = FW(I,J,KF1  )+QW(I,J)*CC(I,J,KF1)
            DO 120 K=KF1+1,MZM
              FW(I,J,K) = FW(I,J,K)+QW(I,J)*CC(I,J,KF1)
  120       CONTINUE         
          END IF
        ELSE
c           srcb(i,j,kf1) = srcb(i,j,kf1) + qw(i,j)*CC(I,J,KF1)*zc(6,kf1)
c           srcb(i,j,kf1) = srcb(i,j,kf1) + qw(i,j)*CC(I,J,KF1)
c     $        /(HX(I,J)-ZC(1,KF1-1))
          FW(I,J,KF1) = FW(I,J,KF1)+QW(I,J)*CC(I,J,KF1)
          DO 130 K=KF1+1,MZM
             FW(I,J,K) = FW(I,J,K)+QW(I,J)*CC(I,J,KF1)
  130     CONTINUE         
        END IF
C
      ENDIF
  100 CONTINUE
C
C----------------------------------------------------------------------
C     SURFACE-TYPE = FUNCTIONの場合
C----------------------------------------------------------------------
      ELSE IF(ISURF(1).EQ.-2) THEN
         DO J=2,MYM
         DO I=2,MXM
            IF( KF(I,J).EQ.KG(I,J) ) THEN
               K = KF(I,J)
               DH = HH(I,J)-HDEP(I,J)
            ELSE
               K = KF(I,J)-1
               DH = GV(I,J,K)*ZC(4,K)
            ENDIF
            SRCB(I,J,K) = -QW(I,J)*CC(I,J,K)/DH
            QWSUM = QWSUM - QW(I,J)*CC(I,J,K)
         ENDDO
         ENDDO
      ENDIF
C
      RETURN
      END
