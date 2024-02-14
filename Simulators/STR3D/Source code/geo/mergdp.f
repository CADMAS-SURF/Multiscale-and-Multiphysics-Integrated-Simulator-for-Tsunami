      SUBROUTINE MERGDP(PG,DPG,DP,INDOP,IELM,NM,IBEL,IMPC,NNOD,NNODC
     &                 ,NELM)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PG(*),DPG(NNOD),DP(*),INDOP(NNOD),IELM(NM,NELM)
     &         ,IBEL(NELM)

      DO I = 1, NNOD
        IFR = INDOP(I)
        IF( IFR > 0 ) THEN
          DPG(I) = DP(IFR)
        ELSEIF( IFR == 0 ) THEN
          DPG(I) = 0.
        ENDIF
      ENDDO

      PG(1:NNOD) = PG(1:NNOD) + DPG(:)

      IF( IMPC > 0 ) CALL GSURF(PG(NNOD+NNODC+1),PG,IELM,NM,NELM,IBEL,1)

      END
