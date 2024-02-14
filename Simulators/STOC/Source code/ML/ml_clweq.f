      SUBROUTINE CLWEQ(WW,HU,HV,XC,YC,ZC,YCOS,YCOSP,GZ,INDW,KF)
C======================================================================
C     連続の式から流速Wを計算する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'VVMAX.h'
C
      REAL(8),INTENT(INOUT)::WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY)
      REAL(8),INTENT(INOUT)::GZ(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY)
C
      INTEGER::I,J,K
C
C
      DO 100 K=1,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDW(I,J,K).GT.0 ) THEN
            IF( K.LT.KF(I,J) ) THEN
               WW(I,J,K) = ( GZ(I,J,K-1)*WW(I,J,K-1)
     $                   -   ZC(4,K)*((HU(I,J,K)-HU(I-1,J,K))*XC(6,I,J)
     $                   +    (HV(I,J,K)-HV(I,J-1,K))*YC(6,J)/YCOS(J)) )
     $                   / GZ(I,J,K)
               IF(ABS(WW(I,J,K)).GT.VVMAX) THEN
                  WW(I,J,K) = SIGN(VVMAX,WW(I,J,K))
               END IF
            ELSE
               WW(I,J,K) = WW(I,J,K-1)
            END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
