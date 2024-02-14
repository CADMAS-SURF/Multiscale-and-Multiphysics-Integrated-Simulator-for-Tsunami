      SUBROUTINE BLOCK(GV,GX,GY,INDP,KBLC)
C======================================================================
C     閉塞処理に関するメインルーチン
C
C
C       KBLC(NI,NJ) 閉塞フラグ
C                     =   0  :  閉塞なし
C                     =   K  :  閉塞
C                               漂流物底面(接触判定用)が含まれる流体側のセルインデックスK
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'BLOCK.h'
C
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::KBLC(MX,MY)
C
C
      REAL(8)::HTWK
      REAL(8)::AWK1,AWK2,AWK3
C
      INTEGER::I,J,K
      INTEGER::NN,IDIR
C
C----------------------------------------------------------------------
C     (1) 面透過率の修正(閉塞がない状態を再現)
C----------------------------------------------------------------------
C
      DO 100 K=2,MZM
      DO 100 I=2+NBFRSZ-1,MXM-NBFRSZ
      DO 100 J=2+NBFRSZ-1,MYM-NBFRSZ
C
         IF( J.NE.2+NBFRSZ-1 )THEN
            IF( INDP(I,J,K).EQ.0 .OR. INDP(I+1,J,K).EQ.0 )THEN
               GX(I,J,K) = 1.0D0
            ELSE
               GX(I,J,K) = MIN(GV(I,J,K),GV(I+1,J,K))
               IF( KBLC(I,J).NE.0 .OR. KBLC(I+1,J).NE.0 )THEN
                  IF( K.GE.MAX(KBLC(I,J),KBLC(I+1,J)) )THEN
                     GX(I,J,K) = MIN(GLIMIT,GX(I,J,K))
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
         IF( I.NE.2+NBFRSZ-1 )THEN
            IF( INDP(I,J,K).EQ.0 .OR. INDP(I,J+1,K).EQ.0 )THEN
               GY(I,J,K) = 1.0D0
            ELSE
               GY(I,J,K) = MIN(GV(I,J,K),GV(I,J+1,K))
               IF( KBLC(I,J).NE.0 .OR. KBLC(I,J+1).NE.0 )THEN
                  IF( K.GE.MAX(KBLC(I,J),KBLC(I,J+1)) )THEN
                     GY(I,J,K) = MIN(GLIMIT,GY(I,J,K))
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
  100 CONTINUE
C
C----------------------------------------------------------------------
C     (2) 面透過率の修正(閉塞を再現)
C----------------------------------------------------------------------
C
C
C
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      RETURN
      END
