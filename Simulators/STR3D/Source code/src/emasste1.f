      SUBROUTINE EMASSTE1(EMASS,AMAT,GRID,KN,LUMP,ITO)
C
C     EMASS : OUT : 要素質量行列
C     AMAT  : IN  : 物性テーブル
C     GRID  : IN  : 節点座標
C     KN    : IN  : 要素の構成節点番号
C     LUMP  : IN  : =0:lumped mass, =1:consistent mass
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NG(2),WGT(2),RL(4,4,2),XYZ(3,4),GRID(3,*),KN(4),AMAT(*)
     &         ,EMASS(4,4),HTH(4,4)
      DATA NG  / 1, 4 /
      DATA WGT / .166666666666667D0, .0416666666666667D0 /
      DATA RL  / 4*0.25D0, 12*0.D0
     &         , .58541020D0, .13819660D0, .13819660D0, .13819660D0
     &         , .13819660D0, .58541020D0, .13819660D0, .13819660D0
     &         , .13819660D0, .13819660D0, .58541020D0, .13819660D0
     &         , .13819660D0, .13819660D0, .13819660D0, .58541020D0 /
      DATA IG / 2 /
C
      DO 100 I=1,4
        CALL SHIFT1(XYZ(1,I),GRID(1,KN(I)),3)
  100 CONTINUE
C
      RHO=AMAT(3)
C
      CALL CLEAR1(EMASS,16)
C
      DO 200 I=1,NG(IG)
        IF(LUMP .EQ. 0) THEN
          CALL RMULT2(EMASS,RL(1,I,IG),RHO,4)
        ELSE
          CALL AXB(HTH,RL(1,I,IG),RL(1,I,IG),4,1,4)
          CALL RMULT2(EMASS,HTH,RHO,16)
        ENDIF
  200 CONTINUE
C
      CALL TETVOL(XYZ,V)
      DET=6.D0*V
      IF(LUMP .EQ. 0) THEN
        NT=4
      ELSE
        NT=16
      ENDIF
      CALL RMULT1(EMASS,EMASS,DET*WGT(IG),NT)
C
      RETURN
      END
