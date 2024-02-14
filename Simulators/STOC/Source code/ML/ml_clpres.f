      SUBROUTINE CLPRES(RHOW,PP,TT,CC,GV,HH,PATM,XC,YC,ZC,INDP,KF,KG)
C======================================================================
C     温度と塩素量濃度より密度を計算し、静水圧式より圧力PPを計算する
C     PATM: 表面上の圧力
C     PP  : 圧力(Pa)、このうち圧力境界セルの圧力値を更新する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
C
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TT(MX,MY,MZ),CC(MX,MY,MZ),GV(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::HH(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY)
      INTEGER::IDB=1
C
      INTEGER::I,J,K
C
C----------------------------------------------------------------------
C     (1) 密度を計算する
C----------------------------------------------------------------------
C
      CALL CLDENS(RHOW,TT,CC,GV,ZC,INDP,KF)
C
C----------------------------------------------------------------------
C     (2) 圧力を計算する
C----------------------------------------------------------------------
C
      CALL ZERCLR(PP,MXYZ,0.0D0)
C
      DO 100 K=MZM,2,-1
      DO 100 J=2,MYM
      DO 100 I=2,MXM
        IF(KF(I,J).LT.MZ) THEN
           IF(K.GT.KF(I,J)) THEN
              RHOW(I,J,K)=0.0D0
           ELSE IF(K.EQ.KF(I,J)) THEN
              PP(I,J,K) = PATM(I,J)-(HH(I,J)-ZC(2,K))*RHOW(I,J,K)*GRAV
           ELSE
              PP(I,J,K) = PP(I,J,K+1)-0.5D0*ZC(4,K+1)*RHOW(I,J,K+1)*GRAV
     $                               -0.5D0*ZC(4,K  )*RHOW(I,J,K  )*GRAV
           END IF
        END IF
  100 CONTINUE
C
      RETURN
      END
