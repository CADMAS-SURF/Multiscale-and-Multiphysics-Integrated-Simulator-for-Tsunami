      SUBROUTINE EMASSTRS(EMASS,AMAT,S,GRID,KN,LUMP,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GRID(3,*),KN(2),AMAT(*),EMASS(2,2)
C----&------------------------------------------------------------------
      CALL LENGTH(RL,GRID(1,KN(1)),GRID(1,KN(2)),3)
      EM=AMAT(3)*S*RL
      IF( LUMP .EQ. 0 ) THEN
        EMASS(1,1) = EM*.5
        EMASS(2,1) = EM*.5
      ELSE
        EMASS(1,1) = EM/3.D0
        EMASS(1,2) = EM/6.D0
        EMASS(2,1) = EM/6.D0
        EMASS(2,2) = EM/3.D0
      ENDIF
C
      RETURN
      END
