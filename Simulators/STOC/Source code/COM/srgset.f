C----------------------------------------------------------
      SUBROUTINE  SRGSET (PATM,HH,XC,YC,ZC)
C----------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::PATM(MX,MY),HH(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      INTEGER::I,J
C
C----------------------------------------------------------
      DO 100 J=1,MY
      DO 100 I=1,MX
        IF(HH(I,J).LT.1.0D10) HH(I,J)= ZC(8,MZM)
  100 CONTINUE
C----------------------------------------------------------
      CALL PRSINI (PATM,XC,YC)
C----------------------------------------------------------
      DO 200 J= 2,MYM
      DO 200 I= 2,MXM
        IF(HH(I,J).LT.1.0D10) HH(I,J)= -0.993D0*PATM(I,J)/100.D0
  200 CONTINUE
c
C----------------------------------------------------------
      RETURN
      END
