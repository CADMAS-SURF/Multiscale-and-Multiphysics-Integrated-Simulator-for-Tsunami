      SUBROUTINE BDYFC(FT,SI,IGR,GRAV,IR0,RFRC,GRID,NELM,NM,IELM,AMAT
     &                ,RODA,BARD,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION R0(3),GRID(3,*),IELM(NM,*),KN(20),AMAT(33,*),XYZ(3,20)
     &         ,FC(3,20),GRAV(3),FT(6,*),OMEGA(3),WK(3),RFRC(3),RODA(*)
     &         ,BARD(6,*)
      DATA PI /3.141592654D0/
C----&------------------------------------------------------------------
      IF(IGR .EQ. 0) THEN
        CALL RMULT1(OMEGA,RFRC,2.*PI,3)
        IF(IR0 .EQ. 0) THEN
          CALL CLEAR1(R0,3)
        ELSE
          CALL SHIFT1(R0,GRID(1,IR0),3)
        ENDIF
      ENDIF
C
      DO 100 IENO=1,NELM
C
        ITYP=IELM(2,IENO)
        ND=IELM(3,IENO)
        IMAT=IELM(4,IENO)
        I5=IELM(5,IENO)
        CALL SHIFT0(KN,IELM(8,IENO),ND)
        RHO=AMAT(3,IMAT)
C
        DO 110 I=1,ND
          CALL SHIFT1(XYZ(1,I),GRID(1,KN(I)),3)
  110   CONTINUE
C
        CALL CLEAR1(FC,3*ND)
C
        IF(ITYP .EQ. 2 .OR. ITYP .EQ. 6) THEN
          IF(ND .EQ. 4) THEN
            CALL BDFTE1(FC,XYZ,GRAV,R0,IGR,ITO)
          ELSEIF(ND .EQ. 10) THEN
            CALL BDFTE2(FC,XYZ,GRAV,R0,IGR,ITO)
          ELSEIF(ND .EQ. 6) THEN
            CALL BDFPN1(FC,XYZ,GRAV,R0,IGR,ITO)
          ELSEIF(ND .EQ. 15) THEN
            CALL BDFPN2(FC,XYZ,GRAV,R0,IGR,ITO)
          ELSE
            CALL BDFHX2(FC,XYZ,ND,GRAV,R0,IGR,ITO)
          ENDIF
        ELSEIF(ITYP .EQ. 3) THEN
          CALL BDFTRS(FC,XYZ,RODA(I5),GRAV,R0,IGR,ITO)
        ELSEIF(ITYP .EQ. 4) THEN
          CALL BDFBM(FC,XYZ,BARD(2,I5),BARD(1,I5),GRAV,R0,IGR,ITO)
        ENDIF
C
        IF(IGR .EQ. 1) THEN
          CALL RMULT1(FC,FC,RHO,3*ND)
        ELSE
          DO 120 I=1,ND
            CALL CROSS(OMEGA,FC(1,I),WK)
            CALL CROSS(OMEGA,WK,FC(1,I))
  120     CONTINUE
          CALL RMULT1(FC,FC,-RHO,3*ND)
        ENDIF
C
        CALL FRCADD(ND,FC,KN,SI,FT)
C
  100 CONTINUE
C
      END
