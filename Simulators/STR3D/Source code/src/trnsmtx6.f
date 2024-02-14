      SUBROUTINE TRNSMTX6(EG,ICRD,P,E,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION OP(3),P(3),E(3,4),EG(3,3)
C----&------------------------------------------------------------------
      IF( ICRD .EQ. 1 ) THEN
C
        CALL SHIFT1(EG,E,9)
C
      ELSEIF( ICRD .EQ. 2 ) THEN
C
        CALL SUBVEC(OP,P,E(1,4),3)
        CALL VECML1(OP1,OP,E(1,1),3)
        CALL VECML1(OP2,OP,E(1,2),3)
C
        R=OP1*OP1+OP2*OP2
        IF( R .LT. 1.D-20 ) THEN
          WRITE(ITO,500) P
  500     FORMAT('LOAD POINT IS ON Z-AXIS OF CYLINDRICAL COODINATE'
     &          ,' SYSTEM.'/'X =',1PE12.4,3X,'Y =',E12.4,3X,'Z =',E12.4)
          CALL ERRSTP(13,ITO)
        ENDIF
C
        CALL RMULT5(EG(1,1),OP1,E(1,1),OP2,E(1,2),3)
        CALL DIRCOS(EG(1,1),EG(1,1),3)
        CALL CROSS(E(1,3),EG(1,1),EG(1,2))
        CALL SHIFT1(EG(1,3),E(1,3),3)
C
      ENDIF
C
      RETURN
      END
