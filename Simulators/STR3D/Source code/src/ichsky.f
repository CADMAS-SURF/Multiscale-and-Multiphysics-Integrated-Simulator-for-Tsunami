      SUBROUTINE ICHSKY(D,A,RLOW,NEQ,IDSK,IDCG,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IDSK(NEQ+1),IDCG(*),RLOW(*),A(*),D(NEQ)
C-----------------------------------------------------------------------
      DO 1000 IROW=1,NEQ
        IS=IDSK(IROW)+1
        IE=IDSK(IROW+1)-1
C
C
        DO 1100 I=IS,IE
          ICOL=IDCG(I)
          RLOW(I)=A(I)
          IF(ICOL.EQ.1) GOTO 1100
C
          ISS0=IS
          DO 1500 K=IDSK(ICOL)+1,IDSK(ICOL+1)-1
            KCOL=IDCG(K)
            ISS=ISS0
            DO 1600 KK=ISS,IE
              IF(IDCG(KK).EQ.KCOL) THEN
                RLOW(I)=RLOW(I)-RLOW(K)*RLOW(KK)*D(KCOL)
                ISS0=KK+1
                GOTO 1500
              ELSEIF(IDCG(KK).LT.KCOL) THEN
                GOTO 1600
              ELSE
                ISS0=KK
                GOTO 1500
              ENDIF
 1600       CONTINUE
 1500     CONTINUE
C
 1100   CONTINUE
C
C
        IP=IDSK(IROW)
        RLOW(IP)=A(IP)
C
        DO 1200 K=IS,IE
          KCOL=IDCG(K)
          RLOW(IP)=RLOW(IP)-RLOW(K)*RLOW(K)*D(KCOL)
 1200   CONTINUE
C
        D(IROW)=1.D0/RLOW(IP)
 1000 CONTINUE
C
      RETURN
      END
