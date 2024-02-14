      SUBROUTINE CELLSC(SC,SN,GV,XC,YC,ZC,HH,HDEP,INDP,KF,KG)
C======================================================================
C     セルに含まれるスカラー量を計算する
C       SC: N時刻のスカラー値
C       SN: セルのスカラー量
C       KF,HH : (N)時刻の値
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::SC(MX,MY,MZ),SN(MX,MY,MZ),GV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY)
C
      INTEGER::I,J,K
C
      CALL ZERCLR(SN,MXYZ,0.0D0)
      DO 100 K=1,MZ
      DO 100 J=1,MY
      DO 100 I=1,MX
        IF(INDP(I,J,K).NE.0) THEN
          IF(K.LT.KF(I,J)) THEN 
            SN(I,J,K) = XC(4,I,J)*YC(4,J)*ZC(4,K)*GV(I,J,K)*SC(I,J,K)
          ELSE IF(K.EQ.KF(I,J)) THEN
            IF(K.EQ.KG(I,J)) THEN
              SN(I,J,K) =XC(4,I,J)*YC(4,J)*(HH(I,J)-HDEP(I,J))*SC(I,J,K)
            ELSE
              SN(I,J,K) =XC(4,I,J)*YC(4,J)*(HH(I,J)-ZC(1,K-1))*SC(I,J,K)
            END IF
          ELSE
            SN(I,J,K) = 0.0D0
          END IF
        END IF
 100  CONTINUE
C
      RETURN
      END
