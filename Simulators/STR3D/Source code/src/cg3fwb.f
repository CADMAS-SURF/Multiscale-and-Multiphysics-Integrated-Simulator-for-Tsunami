      SUBROUTINE CG3FWB(D,RLOW,X,WK,NEQ,IDSK,IDCG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(NEQ),RLOW(*),X(NEQ),WK(NEQ),IDSK(NEQ+1),IDCG(*)
C
      DO 1000 I=1,NEQ
        IS=IDSK(I)+1
        IE=IDSK(I+1)-1
        IF(IS.LE.IE) THEN
          DO 1100 J=IS,IE
            X(I)=X(I)-RLOW(J)*X(IDCG(J))
 1100     CONTINUE
        ENDIF
        X(I)=X(I)*D(I)
 1000 CONTINUE
C
      CALL CLEAR1(WK,NEQ)
      DO 2000 J=NEQ,2,-1
        IS=IDSK(J)+1
        IE=IDSK(J+1)-1
        IF(IS.LE.IE) THEN
          DO 2100 I=IS,IE
            WK(IDCG(I))=WK(IDCG(I))+RLOW(I)*X(J)
 2100     CONTINUE
        ENDIF
        X(J-1)=X(J-1)-D(J-1)*WK(J-1)
 2000 CONTINUE
C
      RETURN
      END
