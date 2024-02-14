      SUBROUTINE EMASSHX2(EMASS,AMAT,GRID,KN,ND,LUMP,ITO)
C
C     EMASS : OUT : 要素質量行列
C     AMAT  : IN  : 物性テーブル
C     GRID  : IN  : 節点座標
C     KN    : IN  : 要素の構成節点番号
C     ND    : IN  : 要素の構成節点数
C     LUMP  : IN  : =0:lumped mass, =1:consistent mass
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WG(3,3),XG(3,3),XYZ(3,20),GRID(3,*),KN(ND),P(3,20)
     &         ,AMAT(*),EMASS(ND,ND),H(20),HTH(400)
      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA WG / 2.0D0, 2*0.0D0
     &        , 2*1.0D0, 0.0D0
     &        , 0.55555555555555556D0, 0.88888888888888889D0
     &        , 0.55555555555555556D0 /
C
      IF(ND .EQ. 8) THEN
        NG=2
      ELSE
        NG=3
      ENDIF
      NT=ND*ND
C
      DO 200 I=1,ND
        CALL SHIFT1(XYZ(1,I),GRID(1,KN(I)),3)
  200 CONTINUE
C
      RHO=AMAT(3)
C
      CALL CLEAR1(EMASS,NT)
      VOL=0.
C
      DO 100 I=1,NG
      DO 100 J=1,NG
      DO 100 K=1,NG
        XG1=XG(I,NG)
        XG2=XG(J,NG)
        XG3=XG(K,NG)
        WGT=WG(I,NG)*WG(J,NG)*WG(K,NG)
        CALL SFNHX2(XG1,XG2,XG3,ND,H)
        CALL DERHX2(XG1,XG2,XG3,ND,P)
        CALL DET3(DET,ND,P,XYZ,ITO)
        IF( LUMP .EQ. 0 ) THEN
          IF(ND .EQ. 8) THEN
            CALL RMULT2(EMASS,H,WGT*RHO*DET,ND)
          ELSE
            VOL=VOL+WGT*DET
            CALL RMULT3(EMASS,H,WGT*DET,ND)
          ENDIF
        ELSE
          CALL AXB(HTH,H,H,ND,1,ND)
          CALL RMULT2(EMASS,HTH,WGT*RHO*DET,NT)
        ENDIF
  100 CONTINUE
C
      IF(LUMP .EQ. 0 .AND. ND .GT. 8) THEN
        CALL SUMVEC(EMASST,EMASS,ND)
        CALL RMULT1(EMASS,EMASS,RHO*VOL/EMASST,ND)
      ENDIF
C
      RETURN
      END
