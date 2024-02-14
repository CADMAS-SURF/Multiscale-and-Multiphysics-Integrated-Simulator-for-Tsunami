      SUBROUTINE EDGEDL(DL,POSS,IEDG,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IEDG(6),POS(3,*),XYZ(3,3),V(3),POSS(3)
C----&------------------------------------------------------------------
      CALL SHIFT1(XYZ(1,3),POSS,3)
      IG1=IEDG(1) 
      IG2=IEDG(2)
      CALL SHIFT1(XYZ(1,1),POS(1,IG1),3)
      CALL SHIFT1(XYZ(1,2),POS(1,IG2),3)
C
      CALL LENCD(RT,V,XYZ)
C
      IF( RT .GT. 1.D0 ) THEN
        DL = RT -1.D0
      ELSEIF( RT .LT. 0.D0 ) THEN
        DL = RT
      ELSE
        DL = 0.
      ENDIF
C
      RETURN
      END
