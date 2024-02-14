      SUBROUTINE EMASSBM(EMASS,AMAT,DIM,GRID,ITYPE,KN,LUMP,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GRID(3,*),KN(2),AMAT(*),EMASS(2,2),DIM(*)
      DATA PI /3.141592654D0/
C----&------------------------------------------------------------------
      CALL LENGTH(RL,GRID(1,KN(1)),GRID(1,KN(2)),3)
C
      IF(ITYPE .EQ. 0) THEN
        A=DIM(1)
      ELSEIF( ITYPE .EQ. 1 ) THEN
        A=PI*DIM(1)*DIM(1)
      ELSEIF( ITYPE .EQ. 2 ) THEN
        A=DIM(1)*DIM(2)
      ELSEIF(ITYPE .EQ. 3) THEN
        A=PI*( DIM(1)*DIM(1) - DIM(2)*DIM(2) )
      ENDIF
C
      EM=AMAT(3)*A*RL
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
