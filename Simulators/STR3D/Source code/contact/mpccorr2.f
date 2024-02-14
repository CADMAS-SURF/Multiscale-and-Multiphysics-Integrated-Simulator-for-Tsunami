      SUBROUTINE MPCCORR2(UG,POS,NS,INDOF,IG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,3),UG(6,*),POS(3,*),IG(2),DUG(3),IC(3),INDOF(3)
C----&------------------------------------------------------------------
      CALL SHIFT1(XYZ(1,1),POS(1,IG(1)),3)
      CALL SHIFT1(XYZ(1,2),POS(1,IG(2)),3)
      CALL SHIFT1(XYZ(1,3),POS(1,NS),3)
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
        CALL LENCD(T,DUG,XYZ)
      ELSE
        CALL CORREDGE(DUG,IC,XYZ)
      ENDIF
C
      CALL ADDVEC(UG(1,NS),UG(1,NS),DUG,3)
      CALL ADDVEC(POS(1,NS),POS(1,NS),DUG,3)
C
      RETURN
      END
