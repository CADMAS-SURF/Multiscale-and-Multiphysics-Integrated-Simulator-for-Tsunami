      SUBROUTINE CG3ILU( D,A,NEQ  ,IDSK,IDCG
     1                 ,ISTOH ,ITO)
C
C              SUB FOR INCOMPLETE DECOMPOSITION 
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION  A(ISTOH),IDSK(NEQ+1),IDCG(*)
      DIMENSION  D(NEQ)
      DATA DFAC /1.5D0/
C
      IF(NEQ.EQ.0) RETURN
C
C     DFACO=DFAC
      D(1)=1.0D0/A(1)
C
   40 DO 20 I=2,NEQ
        IP=IDSK(I)
        IS=IP+1
        IE=IDSK(I+1)-1
        C=DFAC*A(IP)
C
        IF(IS.LE.IE) THEN
          DO 10 J=IS,IE
            JP=IDCG(J)
            C=C-A(J)*A(J)*D(JP)
   10     CONTINUE
        END IF
C
   30   IF(C.LT.1.0D-37) THEN
C         WRITE(ITO,'(/6X,A,I8,E15.7)') 'NEGATIVE IN CG3ILU --- I,D='
C    &                                  ,I,1.0D0/C
          DFAC=DFAC+1.0D-1
          IF(DFAC.GT.5.0D0) THEN
C           WRITE(ITO,*) 'DFAC TOO LARGE, PROGRAM STOPPED!'
            WRITE(ITO,*) 'CG METHOD FAILED.'
            CALL ERRSTP(22,ITO)
          END IF
          GO TO 40
        END IF             
        D(I)=1.0D0/C
   20 CONTINUE
C
C     IF(DFAC .NE. DFACO) THEN
C       WRITE(ITO,100) DFACO,DFAC
C 100   FORMAT(/6X,'DFAC CHANGED : ',F7.3,'  ---> ',F7.3/)
C     END IF
      RETURN
      END
