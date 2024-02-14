      SUBROUTINE CHKVAL(SN,HH,HDEP,KF,VAL)
C======================================================================
C     層厚の小さなセルの物理量を修正する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(OUT)::SN(MX,MY,MZ)
      REAL(8),INTENT(IN)::HH(MX,MY),HDEP(MX,MY)
      INTEGER,INTENT(IN)::KF(MX,MY)
      REAL(8),INTENT(IN)::VAL
C
      REAL(8)::DH
      INTEGER::I,J,K
C
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(KF(I,J).LT.MZ) THEN
            DH = HH(I,J)-HDEP(I,J)
            IF( DH.LT.1.0D-4 ) THEN
               SN(I,J,KF(I,J)  ) = VAL
               SN(I,J,KF(I,J)+1) = VAL
            ENDIF
         ENDIF
 100  CONTINUE
C
      RETURN
      END
