      SUBROUTINE VRTXSEPA(ICHK,IP,F,KN,POS,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(4),NOD(4),XYZ(3,4),POS(3,*),F(3)
      LOGICAL VOLIN
C----&------------------------------------------------------------------
      IF(IP .EQ. KN(1)) THEN
        NOD(1)=KN(1)
        NOD(2)=KN(2)
        NOD(3)=KN(3)
        NOD(4)=KN(4)
      ELSEIF(IP .EQ. KN(2)) THEN
        NOD(1)=KN(2)
        NOD(2)=KN(3)
        NOD(3)=KN(1)
        NOD(4)=KN(4)
      ELSEIF(IP .EQ. KN(3)) THEN
        NOD(1)=KN(3)
        NOD(2)=KN(4)
        NOD(3)=KN(1)
        NOD(4)=KN(2)
      ELSEIF(IP .EQ. KN(4)) THEN
        NOD(1)=KN(4)
        NOD(2)=KN(1)
        NOD(3)=KN(3)
        NOD(4)=KN(2)
      ELSE
        WRITE(ITO,'(/X,A)') 'STOP IN SUB. VRTXSEPA!'
        CALL ERRSTP(90,ITO)
      ENDIF
C
      DO I=1,4
        CALL SHIFT1(XYZ(1,I),POS(1,NOD(I)),3)
      ENDDO
C
      IF( VOLIN(XYZ,F,-1.D-10) ) ICHK=1
C
      RETURN
      END
