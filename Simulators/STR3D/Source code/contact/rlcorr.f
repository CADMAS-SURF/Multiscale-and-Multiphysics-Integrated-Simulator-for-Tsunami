      SUBROUTINE RLCORR(RLD,POSS,IELC,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),POS(3,*),IELC(3),E(3),RL(3),RLD(3),POSS(3)
      DATA EPS / 1.D-6 /
C----&------------------------------------------------------------------
      CALL SHIFT1(XYZ(1,4),POSS,3)
      DO 100 I=1,3
        CALL SHIFT1(XYZ(1,I),POS(1,IELC(I)),3)
  100 CONTINUE
C
      CALL AREACD(RL,E,H,XYZ)
C
      IF( RL(1) .GE. EPS .AND. RL(2) .GE. EPS .AND. 
     &    RL(2) .LE. -RL(1)+1.D0-EPS ) THEN
        CALL SHIFT1(RLD,RL,3)
      ELSEIF( RL(1) .LE. EPS .AND. RL(2) .LE. EPS ) THEN
        RLD(1)=EPS
        RLD(2)=EPS
        RLD(3)=1.D0-RLD(1)-RLD(2)
      ELSEIF( RL(1) .GE. 1.D0-2.D0*EPS .AND. 
     &        RL(2) .LE. RL(1)-1.D0+3.D0*EPS ) THEN
        RLD(1)=1.D0-2.D0*EPS
        RLD(2)=EPS
        RLD(3)=1.D0-RLD(1)-RLD(2)
      ELSEIF( RL(2) .GE. 1.D0-2.D0*EPS .AND. 
     &        RL(2) .GE. RL(1)+1.D0-3.D0*EPS ) THEN
        RLD(1)=EPS
        RLD(2)=1.D0-2.D0*EPS
        RLD(3)=1.D0-RLD(1)-RLD(2)
      ELSEIF( RL(1) .LT. EPS ) THEN
        RLD(1)=EPS
        RLD(2)=RL(2)
        RLD(3)=1.D0-RLD(1)-RLD(2)
      ELSEIF( RL(2) .LT. EPS ) THEN
        RLD(1)=RL(1)
        RLD(2)=EPS
        RLD(3)=1.D0-RLD(1)-RLD(2)
      ELSE
        RLD(1)=(1.D0-EPS+RL(1)-RL(2))*.5D0
        RLD(2)=-RLD(1)+1.D0-EPS
        RLD(3)=1.D0-RLD(1)-RLD(2)
      ENDIF
C
      RETURN
      END
