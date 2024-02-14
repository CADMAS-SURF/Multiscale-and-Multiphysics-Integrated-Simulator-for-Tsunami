      SUBROUTINE KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
C======================================================================
C     水面に関する位置インデックスを設定する
C       KP: 圧力境界条件が適用されはじめるセルのz方向セルインデックス
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::HH(MX,MY),FF(MX,MY,MZ),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
C
      INTEGER::I,J,K
C
      DO 50 K=2,MZM
      DO 50 J=2,MYM
      DO 50 I=2,MXM
         IF(INDP(I,J,K).GT.0) THEN
            IF(HH(I,J).GE.ZC(1,K)) THEN
               FF(I,J,K) = 1.0D0
            ELSE IF(HH(I,J).LT.ZC(1,K-1)) THEN
               FF(I,J,K) = 0.0D0
            ELSE
               FF(I,J,K) = (HH(I,J)-ZC(1,K-1))*ZC(6,K)
               KF(I,J) = K
               KP(I,J) = K
            END IF
         END IF
   50 CONTINUE
C <another representation>
C FF(I,J) = MIN(1.0D0,MAX(0.0D0,(HH(I,J)-ZC(1,K-1))*ZC(6,K))
C
C
C----------------------------------------------------------------------
C     圧力の水面境界条件を設定
C----------------------------------------------------------------------
      DO 110 K=MZM,2,-1
         DO 100 J=2,MYM
         DO 100 I=2,MXM
            IF( INDP(I,J,K).GT.0 .AND. K.LT.KF(I,J) ) THEN
C
C ............ 隣り合ったセルの水位が2メッシュ以上異なる場合
               IF( ( INDU(I-1,J,K).GT.0 .AND. K.GT.KF(I-1,J) ) .OR.
     $             ( INDU(I  ,J,K).GT.0 .AND. K.GT.KF(I+1,J) ) .OR.
     $             ( INDV(I,J-1,K).GT.0 .AND. K.GT.KF(I,J-1) ) .OR.
     $             ( INDV(I,J  ,K).GT.0 .AND. K.GT.KF(I,J+1) ) ) THEN
                  KP(I,J) = K
               END IF
            END IF
  100    CONTINUE
  110 CONTINUE
C
      RETURN
      END
