      SUBROUTINE FACESEPA(ISLV,F,FTOL,IFACE,IELC,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION POS(3,*),E(3),IELC(3),V12(3),V13(3),F(3),ISLV(2)
C----&------------------------------------------------------------------
      CALL SUBVEC(V12,POS(1,IELC(2)),POS(1,IELC(1)),3)
      CALL SUBVEC(V13,POS(1,IELC(3)),POS(1,IELC(1)),3)
      CALL CROSS2(V12,V13,E)
      CALL VECML1(FN,F,E,3)
      IF( FN .GE. -FTOL ) THEN
        ISLV(1)=3
        ISLV(2)=IFACE
      ENDIF
C
      RETURN
      END
