      SUBROUTINE CG3ILU2( D,A,B ,NEQ  ,IDSK,IDCG
     1                 ,ITO)
C
C              SUB FOR INCOMPLETE DECOMPOSITION 
C                 ( BLOCKED NONSYMETRIC-SKYLINE MATRIX)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION  A(*),IDSK(NEQ+1),IDCG(*)
      DIMENSION  D(NEQ),B(NEQ)
      DATA DFAC /1.5D0/
C
      DO 60 I=1,NEQ
        IS=IDSK(I)
        IF(A(IS).LT.0.D0) THEN
          B(I)=-B(I)
          IE=IDSK(I+1)-1
          DO 70 J=IS,IE
            A(J)=-A(J)
   70     CONTINUE
        ENDIF
   60 CONTINUE
C
C      DFACO=DFAC
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
            IF(JP.GT.I) GO TO 30
            IS2=IDSK(JP)+1
            IE2=IDSK(JP+1)-1
            IF(IS2.LE.IE2) THEN
              DO 50 K=IS2,IE2
                IF(IDCG(K).GT.I) GOTO 10
                IF(IDCG(K).EQ.I) THEN
                  C=C-A(J)*A(K)*D(JP)
                  GOTO 10
                ENDIF
   50         CONTINUE
            ENDIF
   10     CONTINUE
        END IF
C
   30   IF(C.LT.1.0D-37) THEN
C          WRITE(ITO,'(/6X,A,I8,E15.7)') 'NEGATIVE IN CG3ILU --- I,D='
C     &                                  ,I,1.0D0/C
          DFAC=DFAC+1.0D-1
          IF(DFAC.GT.5.0D0) THEN
C            WRITE(ITO,*) 'DFAC TOO LARGE! PROGRAM STOP'
C            STOP
            WRITE(ITO,*) 'CG METHOD FAILED.'
            CALL ERRSTP(22,ITO)
          END IF
          GO TO 40
        END IF             
        D(I)=1.0D0/C
   20 CONTINUE
C
C      IF(DFAC .NE. DFACO) THEN
C        WRITE(ITO,100) DFACO,DFAC
C  100   FORMAT(/6X,'DFAC CHANGED : ',F7.3,'  ---> ',F7.3/)
C      END IF
      RETURN
      END
