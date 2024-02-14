      SUBROUTINE FLUXPL(FU,FV,FW,LLWALP,LL)
C======================================================================
C     板境界の流束を修正する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::LL
C
      INTEGER::I,IDIR,ITYP,J,K,M,N
C
C----------------------------------------------------------------------
C     (1) 板境界
C----------------------------------------------------------------------
!CDIR NODEP
      DO 100 N=1,MLWALP
         I = LLWALP(1,N)
         J = LLWALP(2,N)
         K = LLWALP(3,N)
         IDIR = LLWALP(4,N)
         M    = LLWALP(5,N)
         IF(LSEDI.EQ.1 .AND. LL.EQ.0)THEN
            ITYP = 0
         ELSE
            ITYP = LLWALP(LL,N)
         ENDIF
C
C ...... 法線方向が±X方向の面
         IF( IDIR.EQ.1 ) THEN
C
C ......... 勾配ゼロのみ
            IF( ITYP.EQ.0 ) THEN
               FU(I,J,K)=0.0D0
C
C ......... 勾配ゼロのみ
            ELSE
               FU(I,J,K)=0.0D0
            END IF
C
C ...... 法線方向が±Y方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
C
C ......... 勾配ゼロのみ
            IF( ITYP.EQ.0 ) THEN
               FV(I,J,K)=0.0D0
C
C ......... 勾配ゼロのみ
            ELSE
               FV(I,J,K)=0.0D0
            END IF
C
C ...... 法線方向が±Z方向の面
         ELSE IF( IDIR.EQ.3 ) THEN
C
C ......... 勾配ゼロのみ
            IF( ITYP.EQ.0 ) THEN
               FW(I,J,K)=0.0D0
C
C ......... 勾配ゼロのみ
            ELSE
               FW(I,J,K)=0.0D0
            END IF
C
         END IF
  100 CONTINUE
C
      RETURN
      END
