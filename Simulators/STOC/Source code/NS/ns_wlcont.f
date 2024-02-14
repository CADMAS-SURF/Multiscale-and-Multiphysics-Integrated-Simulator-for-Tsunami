      SUBROUTINE WLCONT(INDK,INDP,LLWALL,LLWALP)
C======================================================================
C     壁の数を数える
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      INTEGER,INTENT(INOUT)::INDK(MX,MY,MZ),INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
C
      INTEGER::I,IDIR,INB1,ITYP,J,JNB1,K,KNB1,M,N
C
      CALL ZERCLI(INDK,MXYZ,0)
C
C----------------------------------------------------------------------
C     壁面境界(壁関数の境界条件)
C----------------------------------------------------------------------
C
      DO 100 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = LLWALL(6,N)
C
C ...... 法線方向がX方向の面
C
         IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
            INB1 = I
            IF( IDIR.EQ.1 ) INB1 = I+1
            IF( ITYP.EQ.2 ) THEN
              INDK(INB1,J,K) = INDK(INB1,J,K)+1
            END IF
C
C ...... 法線方向がY方向の面
C
         ELSE IF( IDIR.EQ.2 .OR. IDIR.EQ.3 ) THEN
            JNB1 = J
            IF( IDIR.EQ.3 ) JNB1 = J+1
            IF( ITYP.EQ.2 ) THEN
              INDK(I,JNB1,K) = INDK(I,JNB1,K)+1
            END IF
C
C ...... 法線方向がZ方向の面
C
         ELSE IF( IDIR.EQ.4 .OR. IDIR.EQ.5 ) THEN
            KNB1 = K
            IF( IDIR.EQ.5 ) KNB1 = K+1
            IF( ITYP.EQ.2 ) THEN
              INDK(I,J,KNB1) = INDK(I,J,KNB1)+1
            END IF
         END IF
  100 CONTINUE
C
C----------------------------------------------------------------------
C     板境界(壁関数の境界条件)
C----------------------------------------------------------------------
C
      DO 200 N=1,MLWALP
         I = LLWALP(1,N)
         J = LLWALP(2,N)
         K = LLWALP(3,N)
         IDIR = LLWALP(4,N)
         M    = LLWALP(5,N)
         ITYP = LLWALP(6,N)
C
C ...... 板の法線方向が±X方向の場合
         IF( IDIR.EQ.1 ) THEN
           IF( ITYP.EQ.2 ) THEN
             INDK(I  ,J,K) = INDK(I  ,J,K)+1
             INDK(I+1,J,K) = INDK(I+1,J,K)+1
           END IF
C ...... 板の法線方向が±Y方向の場合
         ELSE IF( IDIR.EQ.2 ) THEN
           IF( ITYP.EQ.2 ) THEN
             INDK(I,J  ,K) = INDK(I,J  ,K)+1
             INDK(I,J+1,K) = INDK(I,J+1,K)+1
           END IF
C ...... 板の法線方向が±Z方向の場合
         ELSE IF( IDIR.EQ.3 ) THEN
           IF( ITYP.EQ.2 ) THEN
             INDK(I,J,K  ) = INDK(I,J,K  )+1
             INDK(I,J,K+1) = INDK(I,J,K+1)+1
           END IF
         END IF
  200 CONTINUE
C
      RETURN
      END
