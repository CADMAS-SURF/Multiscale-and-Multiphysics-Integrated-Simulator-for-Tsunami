      SUBROUTINE MPCCORR3(UG,POS,NS,INDOF,IELC)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),UG(6,*),POS(3,*),IELC(3),DUG(3),IC(3),INDOF(3)
C----&------------------------------------------------------------------
      DO 100 I=1,3
        CALL SHIFT1(XYZ(1,I),POS(1,IELC(I)),3)
  100 CONTINUE
      CALL SHIFT1(XYZ(1,4),POS(1,NS),3)
C
      NC=0
      DO 200 I=1,3
        IF( INDOF(I) .EQ. -2 ) THEN
          IC(I)=2
        ELSEIF( INDOF(I) .LE. 0 ) THEN
          IC(I)=1
          NC=NC+1
        ELSE
          IC(I)=0
        ENDIF
  200 CONTINUE
C
      IF( NC .EQ. 0 ) THEN
        CALL CORRFACE1(DUG,XYZ)
      ELSEIF( NC .EQ. 1 ) THEN
        CALL CORRFACE2(DUG,IC,XYZ)
      ELSEIF( NC .EQ. 2 ) THEN
        CALL CORRFACE3(DUG,IC,XYZ)
      ENDIF
C
      CALL ADDVEC(UG(1,NS),UG(1,NS),DUG,3)
      CALL ADDVEC(POS(1,NS),POS(1,NS),DUG,3)
C
      RETURN
      END
